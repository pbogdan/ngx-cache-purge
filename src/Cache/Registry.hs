{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}

module Cache.Registry
  ( CacheRegistry
  , empty
  , add
  , replace
  , replaceWith
  , entries
  , keys
  , size
  , scanCacheDirectory
  )
where

import           Protolude hiding (always, empty, (&))

import           Cache.Content
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Set as Set
import           System.FilePath.Find hiding (find, filePath)

newtype CacheRegistry =
  CacheRegistry (HashMap DomainName (Set CacheEntry))
  deriving (Show)

type instance IxValue CacheRegistry = Set CacheEntry
type instance Index CacheRegistry = DomainName

instance Ixed CacheRegistry where
  ix k f (CacheRegistry m) = CacheRegistry <$> ix k f m
  {-# INLINE ix #-}

instance At CacheRegistry where
  at k f (CacheRegistry m) = CacheRegistry <$> at k f m
  {-# INLINE at #-}

empty :: CacheRegistry
empty = CacheRegistry HashMap.empty

add :: CacheEntry -> CacheRegistry -> CacheRegistry
add entry (CacheRegistry registry) =
  let domain = entry ^. cacheKey ^. keyDomain
      domainEntries = fromMaybe Set.empty (HashMap.lookup domain registry)
  in CacheRegistry $
     HashMap.insert domain (Set.insert entry domainEntries) registry

replace :: DomainName -> Set CacheEntry -> CacheRegistry -> CacheRegistry
replace domain newEntries (CacheRegistry registry) =
  CacheRegistry (HashMap.insert domain newEntries registry)

replaceWith
  :: DomainName
  -> (Set CacheEntry -> Set CacheEntry)
  -> CacheRegistry
  -> CacheRegistry
replaceWith domain f registry =
  replace domain (f (entries domain registry)) registry

entries :: DomainName -> CacheRegistry -> Set CacheEntry
entries  domain (CacheRegistry registry)=
  fromMaybe Set.empty (HashMap.lookup domain registry)

keys  :: CacheRegistry -> [DomainName]
keys (CacheRegistry registry) = HashMap.keys registry

size :: CacheRegistry -> Int
size (CacheRegistry registry) = HashMap.foldr ((+) . Set.size) 0 registry

scanCacheDirectory :: FilePath -> IO CacheRegistry
scanCacheDirectory dir = do
  files <- findWithHandler (const . const $ return []) always filt dir
  foldM go empty files
  where
    filt :: FilterPredicate
    filt = fileType ==? RegularFile
    go :: CacheRegistry -> FilePath -> IO CacheRegistry
    go registry !p = do
      entry <- parseCacheFile p
      case entry of
        Just !ce -> return $ add ce registry
        Nothing -> return registry
