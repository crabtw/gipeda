{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, NondecreasingIndentation, TypeFamilies #-}
module Shake where

import Development.Shake.Fancy hiding (withTempFile)
import qualified Development.Shake.Fancy as S
import Development.Shake.FilePath
import Development.Shake.Classes
import Control.Monad
import qualified Data.Map as M
import qualified Data.Set as Set
import Data.Functor
import Data.List
import Data.Maybe
import System.IO.Extra (newTempFile)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified System.Directory
import System.Directory (getPermissions, setPermissions, setOwnerExecutable)
import System.Environment (getExecutablePath)
import Data.Aeson
import qualified Data.Text as T

import Development.Shake.Gitlib

import Paths hiding (Hash)
import ParentMap
import BenchmarksInCSV
import qualified BenchmarkSettings as S
import JsonUtils
import EmbeddedFiles
import ReportTypes

git :: (CmdResult b) => String -> [String] -> Action b
git gitcmd args = do
    cmdWrap ("git " ++ gitcmd) $ cmd (words "git -C repository" ++ gitcmd : args)

self :: (CmdResult b) => String -> [String] -> Action b
self name args = do
    -- orderOnly ["gipeda"]
    gipeda <- liftIO getExecutablePath
    cmdWrap name $ cmd gipeda name args

gitRange :: Action [String]
gitRange = do
    s <- liftIO $ S.readSettings "gipeda.yaml"
    heads <- readFileLines "site/out/heads.txt"
    Stdout range <- git "log" $ ["--format=%H"] ++ excludePreStart s  ++ heads
    return $ words range

defaultBranch :: Action String
defaultBranch = S.defaultBranch <$> liftIO (S.readSettings "gipeda.yaml")

excludePreStart :: S.Settings -> [String]
excludePreStart s = case S.start s of
    Just hash -> ["^" ++ hash ++ "^@"]
    Nothing   -> []

needIfThere :: [FilePath] -> Action [FilePath]
needIfThere files = do
    existing <- filterM doesFileExist files
    need existing
    return existing

doesLogExist :: LogSource -> Hash -> Action Bool
doesLogExist BareGit    hash = liftAction $ doesGitFileExist "logs" (hash <.> "log")
doesLogExist FileSystem hash = doesFileExist (logsOf hash)
doesLogExist NoLogs     hash = doesFileExist (resultsOf hash)

findPred, findPredOrSelf :: LogSource -> ParentMap -> Hash -> Action (Maybe Hash)
findPredOrSelf logSource m h = do
    ex <- doesLogExist logSource h
    if ex then return (Just h)
          else findPred logSource m h
findPred logSource m h = case M.lookup h m of
    Just h' -> findPredOrSelf logSource m h'
    Nothing -> return Nothing

findRecent :: LogSource -> ParentMap -> Integer -> FilePath -> Action [FilePath]
findRecent _ _ 0 _ = return []
findRecent logSource m n h = do
    pM <- findPred logSource m h
    (h:) <$> case pM of
        Nothing -> return []
        Just p ->  findRecent logSource m (n-1) p

findAll :: LogSource -> ParentMap -> FilePath -> Action [FilePath]
findAll logSource m h = do
    pM <- findPred logSource m h
    (h:) <$> case pM of
        Nothing -> return []
        Just p ->  findAll logSource m p

descendsFromStart :: S.Settings -> Hash -> Action Bool
descendsFromStart s hash = case S.start s of
    Just startHash -> liftAction $ isGitAncestor "repository" startHash hash
    Nothing        -> return True

newtype LimitRecent = LimitRecent ()
    deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
type instance RuleResult LimitRecent = Integer

newtype GetIndexHTMLFile = GetIndexHTMLFile ()
    deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
type instance RuleResult GetIndexHTMLFile = BS.ByteString
newtype GetGipedaJSFile = GetGipedaJSFile ()
    deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
type instance RuleResult GetGipedaJSFile = BS.ByteString
newtype GetInstallJSLibsScript = GetInstallJSLibsScript ()
    deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
type instance RuleResult GetInstallJSLibsScript = BS.ByteString

data LogSource = FileSystem | BareGit | NoLogs deriving Show

determineLogSource :: IO LogSource
determineLogSource = do
    haveLogs <- System.Directory.doesDirectoryExist "logs"
    if haveLogs
    then do
        (Exit _, Stdouterr s) <- cmd "git -C logs rev-parse --is-bare-repository"
        if s == "true\n"
        then return BareGit
        else return FileSystem
    else return NoLogs

shakeMain :: IO ()
shakeMain = do
    logSource <- determineLogSource

    shakeArgs shakeOptions $ do
    defaultRuleGitLib

{-
    "gipeda" *> \out ->  do
        sources <- getDirectoryFiles "src" ["*.hs"]
        need (map ("src" </>) sources)
        cmd "ghc -isrc --make -O src/gipeda.hs -o" out
    want ["gipeda"]
-}

    getLimitRecent <- addOracle $ \(LimitRecent _) -> do
        need ["gipeda.yaml"]
        S.limitRecent <$> liftIO (S.readSettings "gipeda.yaml")

    "reports" ~> do
        hashes <- gitRange
        withLogs <- filterM (doesLogExist logSource) hashes
        need $ map reportOf withLogs
    want ["reports"]

    "summaries" ~> do
        hashes <- gitRange
        withLogs <- filterM (doesLogExist logSource) hashes
        need $ map summaryOf withLogs
    want ["summaries"]

    getIndexHTMLFile <- addOracle $ \(GetIndexHTMLFile _) -> do
        return indexHtmlFile
    "site/index.html" %> \out -> do
        content <- getIndexHTMLFile (GetIndexHTMLFile ())
        liftIO $ do
            marked <- isMarkedFile out
            when marked $ BS.writeFile out content
    want ["site/index.html"]

    getGipedaJSFile <- addOracle $ \(GetGipedaJSFile _) -> do
        return gipedaJSFile
    "site/js/gipeda.js" %> \out -> do
        content <- getGipedaJSFile (GetGipedaJSFile ())
        liftIO $ do
            marked <- isMarkedFile out
            when marked $ BS.writeFile out content
    want ["site/js/gipeda.js"]

    getInstallJSLibsScript <- addOracle $ \(GetInstallJSLibsScript _) -> do
        return installJSLibsScript
    "install-jslibs.sh" %> \out -> do
        content <- getInstallJSLibsScript (GetInstallJSLibsScript ())
        liftIO $ do
            marked <- isMarkedFile out
            when marked $ do
                BS.writeFile out content
                p <- getPermissions out
                setPermissions out (setOwnerExecutable True p)
                cmd "./install-jslibs.sh"
    want ["install-jslibs.sh"]

    "site/out/backlog.txt" %> \ out -> do
        hashes <- gitRange
        withoutLogs <- filterM ((not <$>) . doesLogExist logSource) hashes
        writeFile' out (unlines withoutLogs)
    want ["site/out/backlog.txt"]

    "site/out/head.txt" %> \ out -> do
        alwaysRerun
        def <- defaultBranch
        Stdout stdout <- git "rev-parse" [def]
        writeFileChanged out stdout

    "site/out/heads.txt" %> \ out -> do
        tags <- readFileLines "site/out/tags.txt"
        tagHashes <- forM tags $ \t -> do
            liftAction $ getGitReference "repository" ("refs/tags/" ++ t)

        branches <- readFileLines "site/out/branches.txt"
        branchHashes <- forM branches $ \t -> do
            liftAction $ getGitReference "repository" ("refs/heads/" ++ t)

        def <- defaultBranch
        masterHash <- liftAction $ getGitReference "repository" ("refs/heads/" ++ def)

        let heads = nub $ masterHash : tagHashes ++ branchHashes
        writeFileChanged out $ unlines $ heads


    "site/out/history.csv" %> \out -> do
        heads <- readFileLines "site/out/heads.txt"

        s <- liftIO $ S.readSettings "gipeda.yaml"
        Stdout stdout <- git "log" $ ["--format=%H;%P"] ++ excludePreStart s ++ heads
        writeFileChanged out stdout
    want ["site/out/history.csv"]

    history' <- newCache $ \() -> do
         orderOnly ["site/out/history.csv"]
         liftIO $ ssvFileToMap "site/out/history.csv"
    let history = history' ()
    let pred h = do { hist <- history; findPred logSource hist h }
    let predOrSelf h = do { hist <- history; findPredOrSelf logSource hist h }
    let recent n h = do { hist <- history; findRecent logSource hist n h }
    let allCommitsFrom h = do { hist <- history; findAll logSource hist h }
    let benchmarksAll =  do
            latest' <- readFileLines "site/out/latest.txt"
            case latest' of
                [latest] -> do
                    commits <- allCommitsFrom latest
                    bss <- forM commits $ \h -> do
                                need [resultsOf h]
                                liftIO $ benchmarksInCSVFile (resultsOf h) >>= return . Set.fromList
                    return $ Set.toList $ mconcat bss
                [] -> return []
                _ -> fail "Broken site/out/latest.txt"
    let youngestCommits n = do
            latest' <- readFileLines "site/out/latest.txt"
            case latest' of
                [latest] -> recent n latest
                [] -> return []
                _ -> fail "Broken site/out/latest.txt"

    "site/out/latest.txt" %> \ out -> do
        [head] <- readFileLines "site/out/head.txt"
        latest <- predOrSelf head
        case latest of
            Just hash -> writeFileChanged out hash
            Nothing   -> writeFileChanged out ""

    "site/out/tags.txt" %> \ out -> do
        alwaysRerun

        need ["gipeda.yaml"]
        s <- liftIO $ S.readSettings "gipeda.yaml"
        case S.interestingTags s of
            Nothing ->
                writeFileChanged out ""
            Just pattern -> do
                Stdout tags <- git "tag" ["-l", pattern]
                tags' <- filterM (descendsFromStart s) (lines tags)
                writeFileChanged out (unlines tags')

    "site/out/branches.txt" %> \ out -> do
        alwaysRerun

        need ["gipeda.yaml"]
        s <- liftIO $ S.readSettings "gipeda.yaml"
        case S.interestingBranches s of
            Nothing ->
                writeFileChanged out ""
            Just pattern -> do
                Stdout branches <- git "branch" ["--list", pattern]
                def <- defaultBranch
                branches <- filterM (descendsFromStart s) (map (drop 2) $ lines branches)
                branches <- filterM (\b -> liftAction $ not <$> isGitAncestor "repository" b def) branches
                writeFileChanged out (unlines branches)

    "graphs" ~> do
        b <- benchmarksAll
        need (map graphFile b)
    want ["graphs"]

    case logSource of
        BareGit ->
            "site/out/results/*.csv" %> \out -> do
                let hash = takeBaseName out
                withTempFile $ \fn -> do
                    log <- liftAction $ readGitFile "logs" (hash <.> "log")
                    liftIO $ BS.writeFile fn log
                    Stdout csv <- cmdWrap "log2csv" $ cmd "./log2csv" fn
                    writeFile' out csv
        FileSystem ->
            "site/out/results/*.csv" %> \out -> do
                let hash = takeBaseName out
                need [logsOf hash]
                Stdout csv <- cmdWrap "log2csv" $ cmd "./log2csv" (logsOf hash)
                writeFile' out csv
        NoLogs -> return ()

    "site/out/graphs//*.json" %> \out -> do
        let bench = dropDirectory1 (dropDirectory1 (dropDirectory1 (dropExtension out)))

        limitRecent <- getLimitRecent (LimitRecent ())
        r <- youngestCommits limitRecent
        need (map reportOf r)

        Stdout json <- self "GraphReport" (bench : r)
        writeFile' out json

    "site/out/branches//*.mergebase" %> \out -> do
        let branch = dropDirectory1 (dropDirectory1 (dropDirectory1 (dropExtension out)))
        def <- defaultBranch
        mb <- liftAction $ getGitMergeBase "repository" ("refs/heads/"++def) ("refs/heads/"++branch)
        writeFile' out mb

    "site/out/branches//*.json" %> \out -> do
        let branch = dropDirectory1 (dropDirectory1 (dropDirectory1 (dropExtension out)))

        branchHead <- liftAction $ getGitReference "repository" ("refs/heads/" ++ branch)
        branchHeadM <- predOrSelf branchHead

        mergeBase <- readFile' $ branchMergebaseOf branch
        mergeBaseM <- predOrSelf mergeBase

        case (branchHeadM, mergeBaseM) of
            (Just branchHead, Just mergeBase) -> do
                need [resultsOf branchHead, resultsOf mergeBase]
                Stdout json <- self "BranchReport" [branch, branchHead, mergeBase]
                writeFile' out json
            _ -> do
                -- Write out nothing here, to ignore the branch
                liftIO $ LBS.writeFile out (encode emptyGlobalReport)

    "site/out/reports/*.json" %> \out -> do
        let hash = takeBaseName out
        need [resultsOf hash]

        pred <- pred hash
        need [resultsOf h | Just h <- return pred]

        Stdout json <- self "RevReport" (hash : [h | Just h <- return pred])
        writeFile' out json

    "site/out/summaries/*.json" %> \out -> do
        let hash = takeBaseName out
        need [reportOf hash]

        Stdout json <- self "Summary" [hash]
        writeFile' out json

    "site/out/latest-summaries.json" %> \out -> do
        limitRecent <- getLimitRecent (LimitRecent ())
        recentCommits <- youngestCommits limitRecent

        tags <- readFileLines "site/out/tags.txt"
        tagsHashes <- forM tags $ \t -> do
            liftAction $ getGitReference "repository" ("refs/tags/" ++ t)

        branches <- readFileLines "site/out/branches.txt"
        branchHashes <- forM branches $ \branch -> do
            liftAction $ getGitReference "repository" ("refs/heads/" ++ branch)

        need $ map branchSummaryOf branches
        branchesData <- forM branches $ \branch -> do
            json <- liftIO $ LBS.readFile (branchSummaryOf branch)
            case eitherDecode json of
                Left e -> fail e
                Right rep -> return (rep :: Value)

        let o = object
                [ T.pack "tags" .= object [ (T.pack t .= h) | (t,h) <- zip tags tagsHashes ]
                ]
        liftIO $ LBS.writeFile out (encode o)
        extraCommits <- catMaybes <$> mapM predOrSelf (tagsHashes ++ branchHashes)

        let revs = nub $ recentCommits ++ extraCommits

        need $ map summaryOf revs

        g <- forM revs $ \rev -> do
            json <- liftIO $ LBS.readFile (summaryOf rev)
            case eitherDecode json of
                Left e -> fail e
                Right rep -> return (rep :: Value)

        liftIO $ LBS.writeFile out (encode (merges (o : branchesData ++ g)))
    want ["site/out/latest-summaries.json"]

    "site/out/graph-summaries.json" %> \out -> do
        b <- benchmarksAll
        need (map graphFile b)

        Stdout json <- self "GraphSummaries" b
        writeFile' out json
    want ["site/out/graph-summaries.json"]

    "site/out/benchNames.json" %> \out -> do
        b <- benchmarksAll

        need ["gipeda.yaml"]

        Stdout json <- self "BenchNames" b
        writeFile' out json
    want ["site/out/benchNames.json"]


    "site/out/all-summaries.json" %> \out -> do
        hashes <- gitRange
        revs <- filterM (doesLogExist logSource) hashes
        need (map summaryOf revs)

        g <- forM revs $ \rev -> do
            json <- liftIO $ LBS.readFile (summaryOf rev)
            case eitherDecode json of
                Left e -> fail e
                Right rep -> return (rep :: Value)
        liftIO $ LBS.writeFile out (encode (merges g))
    want ["site/out/all-summaries.json"]

    "site/out/settings.json" %> \out -> do
        need ["gipeda.yaml"]

        Stdout json <- self "JsonSettings" []
        writeFile' out json
    want ["site/out/settings.json"]

    phony "clean" $ do
        removeFilesAfter "site/out" ["//*"]

-- | Create a temporary file in the temporary directory. The file will be deleted
--   after the action completes (provided the file is not still open).
--   The 'FilePath' will not have any file extension, will exist, and will be zero bytes long.
--   If you require a file with a specific name, use 'withTempDir'.
withTempFile :: (FilePath -> Action a) -> Action a
withTempFile act = do
    (file, del) <- liftIO newTempFile
    act file `actionFinally` del
