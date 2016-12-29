{-# LANGUAGE FlexibleContexts #-}

module ThreadPool
  ( ThreadPool
  , mkThreadPool
  , runInPool
  )

where

import Protolude.Lifted

import Control.Monad.Trans.Control

data ThreadPool = ThreadPool
  { threadPoolSem :: !QSem
  }

mkThreadPool
  :: MonadBaseControl IO m
  => Int -> m ThreadPool
mkThreadPool size = do
  sem <- newQSem size
  return $ ThreadPool sem

runInPool
  :: MonadBaseControl IO m
  => m a -> (a -> m ()) -> (SomeException -> m ()) -> ThreadPool -> m ()
runInPool action onRight onLeft pool = do
  let sem = threadPoolSem pool
  _ <- waitQSem sem
  _ <-
    fork $ do
      a <- async action
      r <- waitCatch a
      case r of
        Right x -> onRight x
        Left e -> onLeft e
      signalQSem sem
  return ()
