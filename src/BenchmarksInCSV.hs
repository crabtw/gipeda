module BenchmarksInCSV where

import Data.Csv hiding (encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Vector as V
import Data.Char
import Data.Functor

type Benchmark = String

benchmarksInCSVFile :: FilePath -> IO [Benchmark]
benchmarksInCSVFile fname = benchmarksInCSV <$> BS.readFile fname

benchmarksInCSV :: BS.ByteString -> [Benchmark]
benchmarksInCSV s =
    [ n
    | (n,v) <- either error V.toList $ decodeWith ssv NoHeader $ LBS.fromStrict s
    , const True (v::String)
    ]

ssv :: DecodeOptions
ssv = defaultDecodeOptions {
    decDelimiter = fromIntegral (ord ';')
}
