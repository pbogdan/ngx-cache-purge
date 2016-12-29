module Main where

import CachePurge
import Control.Monad.Logger
import System.Remote.Monitoring

main :: IO ()
main = do
  _ <- forkServer "localhost" 8000
  runStderrLoggingT loop
