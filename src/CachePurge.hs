{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module CachePurge
  (loop)

where

import           Protolude.Lifted

import           Cache.Content
import           Cache.Purge
import           Cache.Registry (CacheRegistry)
import qualified Cache.Registry as Registry
import           Control.Concurrent.STM
import           Control.Monad.Logger
import           Control.Monad.Trans.Control
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Database.Redis as Redis
import qualified JobQueue as Queue
import           JobQueue.Types
import qualified Notify
import           ThreadPool (ThreadPool)
import qualified ThreadPool as Pool

loop
  :: forall (m :: * -> *).
     (MonadBaseControl IO m, MonadLogger m, MonadIO m)
  => m ()
loop = do
  conn <- liftIO $ Redis.connect Redis.defaultConnectInfo
  registry <- liftIO $ newTVarIO =<< Registry.scanCacheDirectory "/mnt/cache"
  jobs <- liftIO $ Queue.jobQueue conn
  watcher <- Notify.watcher "/mnt/cache/"
  pool <- Pool.mkThreadPool 5
  a <- async $ void $ forever $ watchFileEvents watcher registry
  b <- async $ void $ forever $ watchJobQueue jobs pool registry
  let c = Notify.getAsync watcher
  (_, errOrRet) <-
    waitAnyCatchCancel [a, b, c] :: m (Async (StM m ()), Either SomeException ())
  case errOrRet of
    Right () -> return ()
    Left e -> do
      logErrorN $ "Thread aborted with an exception: " <> Text.pack (show e)
      liftIO $ Notify.kill watcher
      liftIO $ Queue.kill jobs

watchFileEvents
  :: (MonadBaseControl IO m, MonadLogger m, MonadIO m)
  => Notify.Watcher n -> TVar CacheRegistry -> m ()
watchFileEvents watcher registry = do
  event <- liftIO $ Notify.getEvent watcher
  case event of
    Notify.InotifyEvent path -> do
      void $
        fork $ do
          entryOrErr <- try (liftIO $ parseCacheFile path)
          case entryOrErr of
            Right (Just !entry) ->
              liftIO $ atomically $ modifyTVar' registry $ Registry.add entry
            Right Nothing ->
              logInfoN $ "Cache key not found in file " <> Text.pack path
            Left (e :: IOException) ->
              logWarnN $
              "Exception while parsing file " <> Text.pack path <> ": " <>
              Text.pack (show e)
      return ()
    Notify.InotifyError err -> logWarnN $ "Inotify error: " <> err

watchJobQueue
  :: (MonadLogger m, MonadIO m, MonadBaseControl IO m)
  => Queue.JobQueue -> ThreadPool -> TVar CacheRegistry -> m ()
watchJobQueue jobs pool registry = do
  event <- liftIO $ Queue.getJob jobs
  case event of
    Queue.JobQueueEvent job -> do
      Pool.runInPool
        (processPurgeJob job registry)
        (const (return ()))
        (\e -> logWarnN $ "Exception in purge: " <> (Text.pack . show $ e))
        pool
      return ()
    Queue.JobQueueError err -> logWarnN $ "Inotify error: " <> err

processPurgeJob
  :: (MonadLogger m, MonadIO m)
  => PurgeJob -> TVar CacheRegistry -> m ()
processPurgeJob job registry = do
  logInfoN $ "Received a purge job: " <> Text.pack (show job)
  current <- liftIO $ atomically $ readTVar registry
  logInfoN $ "Total cache entries: " <> Text.pack (show (Registry.size current))
  let maybeEntries = current ^. at (DomainName (pjHost job))
  case maybeEntries of
    Just entries -> do
      (good, _bad) <- liftIO $ purge (pjPath job) entries
      logInfoN $ "Finshed purge: " <> Text.pack (show job)
      liftIO . atomically . modifyTVar registry $
        Registry.replaceWith (DomainName (pjHost job)) (Set.\\ good)
      v' <- liftIO $ atomically $ readTVar registry
      logInfoN $
        "Total cache entries after purge: " <>
        (Text.pack . show $ Registry.size v')
      return ()
    Nothing -> do
      logWarnN $ "Domain not found " <> Text.pack (show (pjHost job))
      fmap
        logDebugN
        (mappend "Known domains:" . Text.pack . show)
        (Registry.keys current)
      return ()
