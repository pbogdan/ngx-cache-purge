{-# LANGUAGE DeriveGeneric #-}
module Main where

import Cache.Header
import Protolude
import Text.Show.Pretty (pPrint)

main :: IO ()
main = do
  (path : _) <- getArgs
  headerOrErr <- parseCacheFileHeader path
  case  headerOrErr of
    Right header -> pPrint header
    Left e -> putText $ "an error occurred: " <> show e
