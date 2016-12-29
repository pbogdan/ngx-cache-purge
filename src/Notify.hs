{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Notify
  ( Watcher
  , watcher
  , kill
  , getEvent
  , getAsync
  , InotifyEvent(..)
  )

where

import           Protolude.Lifted hiding (always)

import           Control.Concurrent.STM hiding (always)
import           Control.Monad.Trans.Control
import qualified Data.ByteString as Bytes
import qualified Data.ByteString.Char8 as CharBytes
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           System.FilePath ((</>))
import           System.FilePath.Find hiding (find,filePath, Unknown)
import           System.INotify

newtype WatchList =
  WatchList (HashMap FilePath WatchDescriptor)

lookupWatch
  :: MonadIO m
  => TVar WatchList -> FilePath -> m (Maybe WatchDescriptor)
lookupWatch watches path = do
  WatchList oldWatches <- liftIO $ atomically $ readTVar watches
  let watch = HashMap.lookup path oldWatches
  return watch

insertWatch
  :: MonadIO m
  => TVar WatchList -> FilePath -> WatchDescriptor -> m ()
insertWatch watches path wd = do
  WatchList oldWatches <- liftIO $ atomically $ readTVar watches
  _ <-
    liftIO $
    atomically $
    swapTVar watches (WatchList $ HashMap.insert path wd oldWatches)
  return ()

deleteWatch
  :: MonadIO m
  => TVar WatchList -> FilePath -> m ()
deleteWatch watches path = do
  WatchList oldWatches <- liftIO $ atomically $ readTVar watches
  _ <-
    liftIO $
    atomically $ swapTVar watches (WatchList $ HashMap.delete path oldWatches)
  return ()

data Watcher m = Watcher
  { _root :: FilePath
  , _inotify :: INotify
  , _watches :: TVar WatchList
  , _queue :: TBQueue InotifyEvent
  , _async :: Async (StM m ())
  }

-- instance Show Watcher where
--     show = _root

setupWatches
  :: MonadIO m
  => INotify
  -> [EventVariety]
  -> (FilePath -> Event -> IO ())
  -> FilePath
  -> m WatchList
setupWatches inotify evs cb root = do
  dirs <- liftIO $ findWithHandler ignoreErrors always filterPredicate root
  liftIO $ foldlM go (WatchList HashMap.empty) dirs
  where
    ignoreErrors :: FilePath -> IOException -> IO [FilePath]
    ignoreErrors _ _ = return []
    filterPredicate :: FilterPredicate
    filterPredicate = fileType ==? Directory
    go :: WatchList -> FilePath -> IO WatchList
    go (WatchList acc) path = do
      watch <- addWatch inotify evs path (cb path)
      return $ WatchList $ HashMap.insert path watch acc

handler :: TBQueue (FilePath, Event) -> FilePath -> Event -> IO ()
handler !queue !path !event = atomically $ writeTBQueue queue (path, event)

watcher
  :: (MonadBaseControl IO m, MonadIO m)
  => FilePath -> m (Watcher m)
watcher root = do
  notify <- liftIO initINotify
  queueSize <-
    fromMaybe 16384 . readMaybe . CharBytes.unpack <$>
    liftIO (Bytes.readFile "/proc/sys/fs/inotify/max_queued_events")
  q1 <-
    liftIO $
    atomically (newTBQueue queueSize :: STM (TBQueue (FilePath, Event)))
  q2 <- liftIO $ atomically (newTBQueue queueSize :: STM (TBQueue InotifyEvent))
  ws <- liftIO $ atomically $ newTVar (WatchList HashMap.empty)
  watches <- setupWatches notify [MoveIn, Close] (handler q1) root
  a <- async (pollingThread notify ws q1 q2)
  _ <- liftIO $ atomically $ swapTVar ws watches
  return $ Watcher root notify ws q2 a

-- @TODO: should I drain the queue as well?
kill :: Watcher m -> IO ()
kill Watcher {..} = do
  _ <- cancel _async
  WatchList ws <- atomically $ readTVar _watches
  for_ ws removeWatch
  killINotify _inotify

data InotifyException =
  InotifyQueueOverflow
  deriving (Show)

instance Exception InotifyException

data InotifyEvent
  = InotifyEvent FilePath
  | InotifyError Text
  deriving (Show)

getAsync :: Watcher m -> Async (StM m ())
getAsync Watcher {..} = _async

getEvent :: Watcher m -> IO InotifyEvent
getEvent w = atomically $ readTBQueue (_queue w)

safeAddWatch
  :: MonadIO m
  => INotify
  -> [EventVariety]
  -> FilePath
  -> (FilePath -> Event -> IO ())
  -> m (Either Text WatchDescriptor)
safeAddWatch inotify evs path cb =
  liftIO $
  catch
    (Right <$> addWatch inotify evs path (cb path))
    (\e -> return . Left $ show (e :: IOException))

pollingThread
  :: (MonadBaseControl IO m, MonadIO m)
  => INotify
  -> TVar WatchList
  -> TBQueue (FilePath, Event) -- ^ queue of events from inotify
  -> TBQueue InotifyEvent -- ^ the output queue
  -> m ()
pollingThread inotify watches inq outq = do
  (path, event) <- liftIO $ atomically $ readTBQueue inq
  maybeE <-
    catch
      (processInotifyEvent event path watches inotify inq)
      (\e -> return (Just (InotifyError (show (e :: IOException)))))
  case maybeE of
    Nothing -> return ()
    Just e -> liftIO $ atomically $ writeTBQueue outq e
  pollingThread inotify watches inq outq

eventPath :: Event -> Maybe FilePath
eventPath Accessed {..} = maybeFilePath
eventPath Modified {..} = maybeFilePath
eventPath Attributes {..} = maybeFilePath
eventPath Closed {..} = maybeFilePath
eventPath Opened {..} = maybeFilePath
eventPath MovedOut {..} = Just filePath
eventPath MovedIn {..} = Just filePath
eventPath Created {..} = Just filePath
eventPath Deleted {..} = Just filePath
eventPath MovedSelf {} = Nothing
eventPath DeletedSelf {} = Nothing
eventPath Unmounted {} = Nothing
eventPath QOverflow {} = Nothing
eventPath Ignored {} = Nothing
eventPath (Unknown _) = Nothing

processInotifyEvent
  :: (MonadBaseControl IO m, MonadIO m)
  => Event
  -> FilePath
  -> TVar WatchList
  -> INotify
  -> TBQueue (FilePath, Event)
  -> m (Maybe InotifyEvent)
processInotifyEvent event path watches inotify inq = do
  when (event == QOverflow) $ throwIO InotifyQueueOverflow
  when (shouldRemove event) $ do
    _ <- fmap removeWatch <$> lookupWatch watches (path </> filePath event)
    deleteWatch watches (path </> filePath event)
  when (shouldAdd event) $
    either (liftIO . print) (insertWatch watches (path </> filePath event)) =<<
    safeAddWatch inotify [MoveIn, Close] (path </> filePath event) (handler inq)
  case (eventPath event, isFile event && shouldReport event) of
    (Just !subpath, True) -> return (Just (InotifyEvent (path </> subpath)))
    (Just !_, False) -> return Nothing
    (Nothing, !_) -> return Nothing

isDeleted :: Event -> Bool
isDeleted Deleted {} = True
isDeleted _ = False

isCreated :: Event -> Bool
isCreated Created {} = True
isCreated _ = False

isMovedIn :: Event -> Bool
isMovedIn MovedIn {} = True
isMovedIn _ = False

isMovedOut :: Event -> Bool
isMovedOut MovedOut {} = True
isMovedOut _ = False

shouldReport :: Event -> Bool
shouldReport Closed {..} = wasWriteable
shouldReport MovedIn {} = True
shouldReport _ = False

shouldRemove :: Event -> Bool
shouldRemove e =
  (isDeleted e && isDirectory e) || (isMovedOut e && isDirectory e)

shouldAdd :: Event -> Bool
shouldAdd e =
  (isCreated e && isDirectory e) || (isMovedIn e && isDirectory e)

isFile :: Event -> Bool
isFile DeletedSelf = False
isFile Unmounted = False
isFile QOverflow = False
isFile Ignored = False
isFile (Unknown _) = False
isFile x = not . isDirectory $ x
