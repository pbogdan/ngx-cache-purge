{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}

module Cache.Registry.Mutable
  ( CacheRegistry
  -- , empty
  , add
  , replace
  , replaceWith
  , replaceWithM
  , entries
  , keys
  , size
  , scanCacheDirectory
  )
where

import           Protolude hiding (always, empty, (&))

import           Cache.Content
import           Control.Concurrent.STM hiding (always)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Set as Set
import           System.FilePath.Find hiding (find, filePath)


newtype CacheRegistry =
  CacheRegistry (HashMap DomainName (TVar (Set CacheEntry)))

empty :: Monad m => m CacheRegistry
empty = return $ CacheRegistry HashMap.empty

add :: CacheEntry -> CacheRegistry -> STM CacheRegistry
add entry r@(CacheRegistry registry) = do
  let domain = entry ^. cacheKey ^. keyDomain
  t <- entries domain r
  modifyTVar' t $ Set.insert entry
  return $ CacheRegistry (HashMap.insert domain t registry)

replace :: DomainName -> Set CacheEntry -> CacheRegistry -> STM CacheRegistry
replace domain newEntries (CacheRegistry registry) = do
  t <- newTVar newEntries
  return $ CacheRegistry (HashMap.insert domain t registry)

replaceWith
  :: DomainName
  -> (Set CacheEntry -> Set CacheEntry)
  -> CacheRegistry
  -> STM CacheRegistry
replaceWith domain f registry = do
  old <- readTVar =<< entries domain registry
  replace domain (f old) registry

replaceWithM
  :: MonadIO m
  => DomainName
  -> (Set CacheEntry -> m (Set CacheEntry))
  -> CacheRegistry
  -> m CacheRegistry
replaceWithM domain f registry = do
  es <- liftIO . atomically $ entries domain registry
  old <- liftIO . atomically . readTVar $ es
  new <- f old
  liftIO . atomically $ replace domain new registry


entries :: DomainName -> CacheRegistry -> STM (TVar (Set CacheEntry))
entries domain (CacheRegistry registry) = do
  let maybeEntries = HashMap.lookup domain registry
  case maybeEntries of
    Nothing -> newTVar Set.empty
    Just x -> return x

keys  :: CacheRegistry -> [DomainName]
keys (CacheRegistry registry) = HashMap.keys registry

size :: DomainName -> CacheRegistry -> STM Int
size domain registry = do
  es <- readTVar =<< entries domain registry
  return $ Set.size es

scanCacheDirectory :: MonadIO m => FilePath -> m CacheRegistry
scanCacheDirectory dir = do
  files <- liftIO $ findWithHandler (const . const $ return []) always filt dir
  e <- empty
  foldM go e files
  where
    filt :: FilterPredicate
    filt = fileType ==? RegularFile
    go registry !p = do
      entry <- liftIO $ parseCacheFile p
      case entry of
        Just !ce -> liftIO $ atomically $ add ce registry
        Nothing -> return registry
