{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cache.Purge
  (  MonadPurge(..)
  ) where

import           Protolude

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC
import           Data.ByteString.Search
import           Data.ByteString.Search.Substitution
import qualified Data.Set as Set
import qualified Data.Set.Extra as Set
import           System.Directory
import           Text.Regex.Extra
import           Text.Regex.PCRE.Light

import           Cache.Content

data PurgeKind
  = FULL !ByteString
  | EXACT !ByteString
  | REGEX !ByteString
  deriving (Show)

type FilterFunction = CacheEntry -> Bool

class MonadPurge m where
  purge :: ByteString -> Set CacheEntry -> m (Set CacheEntry, Set CacheEntry)

instance MonadPurge IO where
  purge = purgeIO

instance MonadPurge Identity where
  purge p c = Identity $ purgePure p c

purgeIO :: ByteString -> Set CacheEntry -> IO (Set CacheEntry, Set CacheEntry)
purgeIO pjp ces =
  let filt = purgeFilter (parsePurge pjp)
  in Set.partitionM nuke (Set.filter filt ces)
  where
    nuke :: CacheEntry -> IO Bool
    nuke ce =
      catch
        (removeFile (BC.unpack (ce ^. filePath)) >> return True)
        (\(_ :: SomeException) -> return False)

purgePure :: ByteString -> Set CacheEntry -> (Set CacheEntry, Set CacheEntry)
purgePure pjp ces =
  let filt = purgeFilter (parsePurge pjp)
      del = const $ pure True
  in runIdentity $ Set.partitionM del (Set.filter filt ces)

parsePurge :: ByteString -> PurgeKind
parsePurge p@((==) "/(.*)" -> True) = FULL p
parsePurge p@((==) 36 . BS.last -> True) = EXACT p
parsePurge p = REGEX p

purgeFilter :: PurgeKind -> FilterFunction
purgeFilter (FULL _) _ = True
purgeFilter (EXACT p) ce = ce ^. cacheKey . keyPath == p
purgeFilter (REGEX p) ce =
  isJust (match (purgeRegex p) (ce ^. cacheKey . keyPath) [])

purgeRegex :: ByteString -> Regex
purgeRegex p =
  compile ("^" `BS.append` prepareRegex p `BS.append` "(\\?.*)?$") []

prepareRegex :: ByteString -> ByteString
prepareRegex =
  r "@@" ("/?" :: ByteString) .
  r "@@@@" ("(.*)" :: ByteString) .
  escape . r "/?" ("@@" :: ByteString) . r "(.*)" ("@@@@" :: ByteString)
  where
    r
      :: Substitution rep
      => ByteString -> rep -> ByteString -> ByteString
    r a b c = BL.toStrict $ replace a b c
