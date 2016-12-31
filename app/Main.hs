module Main where

import Protolude.Lifted

import Control.Monad.Logger
import NgxCachePurge
import System.Remote.Monitoring

main :: IO ()
main = do
  _ <- forkServer "localhost" 8000
  runStderrLoggingT loop
