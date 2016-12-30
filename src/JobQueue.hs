{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module JobQueue
  ( PurgeJob
  , pjHost
  , pjPath
  , JobQueue(..)
  , JobQueueEvent(..)
  , jobQueue
  , kill
  , getJob
  , getJobsQueue
  )

where

import           Protolude

import           Control.Concurrent.STM.TQueue
import qualified Data.ByteString as BS
import qualified Database.Redis as Redis

data PurgeJob = PurgeJob
  { pjHost :: ByteString
  , pjPath :: ByteString
  } deriving (Show, Eq)

data JobQueue = JobQueue
  { _queue :: TQueue JobQueueEvent
  , _redisConn :: Redis.Connection
  , _threadId :: ThreadId
  }

data JobQueueEvent
  = JobQueueEvent PurgeJob
  | JobQueueError Text
  deriving (Eq, Show)


jobQueue :: Redis.Connection -> IO JobQueue
jobQueue conn = do
  queue <- atomically (newTQueue :: STM (TQueue JobQueueEvent))
  tid <- forkIO (pollPurgeQueue conn queue)
  return $ JobQueue queue conn tid

pollPurgeQueue :: Redis.Connection -> TQueue JobQueueEvent -> IO ()
pollPurgeQueue conn queue = do
  j <-
    Redis.runRedis conn $ do
      job <- Redis.blpop ["purge_list"] 0
      case job of
        Right v -> return v
        Left _ -> return Nothing
  case j of
    Just (_, job) -> do
      void $ atomically $ writeTQueue queue (JobQueueEvent $ parseJobString job)
      pollPurgeQueue conn queue
    Nothing -> pollPurgeQueue conn queue
  return ()

explode :: ByteString -> ByteString -> (ByteString, ByteString)
explode sep str =
  let (pre, post) = BS.breakSubstring "::" str
  in (pre, BS.drop (BS.length sep) post)

parseJobString :: ByteString -> PurgeJob
parseJobString s = uncurry PurgeJob $ explode "::" s

kill :: JobQueue -> IO ()
kill JobQueue {..} = killThread _threadId

getJob :: JobQueue -> IO JobQueueEvent
getJob JobQueue {..} = atomically $ readTQueue _queue

getJobsQueue :: JobQueue -> TQueue JobQueueEvent
getJobsQueue = _queue
