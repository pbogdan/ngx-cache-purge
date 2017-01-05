module Main where

import           Protolude.Lifted

import           Cache.Content
import           Criterion.Main
import qualified Data.Attoparsec.ByteString as A
import qualified Data.ByteString as Bytes

testInput :: ByteString
testInput =
  Bytes.concat $
  ["KEY: standard--", "httpsvery-long-domain-name-example.com", "/"] ++
  replicate 5000 "very-long-path-indeed" ++ ["?ver=32432&fsda=3223"]

main :: IO ()
main =
  defaultMain
    [ bgroup
        "Cache.Content"
        [ bench "parseCacheKey" $ nf (A.parseOnly parseCacheKey) testInput
        , bench "parseCacheFile" $ nfIO (parseCacheFile "bench/cache-file")
        ]
    ]
