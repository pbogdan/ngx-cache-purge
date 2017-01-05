{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -funbox-strict-fields #-}

module Cache.Content
  ( DomainName(..)
  , CacheKey(..)
  , keyDomain
  , keyPath

  , CacheEntry(..)
  , cacheKey
  , filePath
  , cacheHeader

  , parseCacheFile
  , parseCacheKey

  , module Control.Lens
  )

where

import           Protolude.Lifted

import           Cache.Header
import           Control.Lens
import           Control.Monad.Except
import           Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString as A
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import           Data.Hashable
import           System.IO (withFile, IOMode(..))

newtype DomainName =
  DomainName ByteString
  deriving (Eq, Show, Ord, Generic)

instance Hashable DomainName

instance NFData DomainName

data CacheKey = CacheKey
  { _keyDomain :: !DomainName
  , _keyPath :: !ByteString
  } deriving (Generic, Show, Eq, Ord)

makeLenses ''CacheKey

instance NFData CacheKey

data CacheEntry = CacheEntry
  { _filePath :: !ByteString
  , _cacheHeader :: !CacheHeader
  , _cacheKey :: !CacheKey
  } deriving (Generic, Show, Eq, Ord)

makeLenses ''CacheEntry

instance NFData CacheEntry

parseCacheKey :: Parser CacheKey
parseCacheKey =
  CacheKey <$>
  (DomainName <$> (parsePrefix *> A.takeWhile notSlash <> A.string "/")) <*>
  A.takeByteString
  where
    notSlash :: Word8 -> Bool
    notSlash = (/=) $ toEnum 47

parsePrefix :: Parser ByteString
parsePrefix = foldr (liftA2 (<>)) (pure mempty) actions
  where
    actions =
      [ BS.pack <$>
        A.manyTill
          (A.satisfy $ const True)
          (A.string "--" *> A.skipWhile (\x -> x == toEnum 45))
      , parseProtocol
      ]

hoistEither :: Monad m => Either e a -> ExceptT e m a
hoistEither = ExceptT . return

parseProtocol :: Parser ByteString
parseProtocol =
  A.string "https" <|> A.string "http" <|> A.string "s" <|> A.string ""

parseCacheFile :: FilePath -> IO (Maybe CacheEntry)
parseCacheFile path =
  withFile path ReadMode $ \h -> do
    headerBytes <- BS.hGet h 4096
    entryOrErr <-
      runExceptT $ do
        header <- hoistEither $ parseCacheHeader headerBytes
        key <-
          hoistEither $
          first toS $ A.parseOnly parseCacheKey . cacheHeaderKey $ header
        return $ CacheEntry (BC.pack path) header key
    case entryOrErr of
      Right !x -> return $ Just x
      Left !_ -> return Nothing
