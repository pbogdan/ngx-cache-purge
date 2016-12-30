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

  , parseCacheFile
  , parseCacheKey

  , module Control.Lens
  )

where

import           Protolude.Lifted

import           Control.Lens
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
      [ A.string "KEY: "
      , BS.pack <$>
        A.manyTill
          (A.satisfy $ const True)
          (A.string "--" *> A.skipWhile (\x -> x == toEnum 45))
      , parseProtocol
      ]

parseProtocol :: Parser ByteString
parseProtocol =
  A.string "https" <|> A.string "http" <|> A.string "s" <|> A.string ""

parseCacheFile :: FilePath -> IO (Maybe CacheEntry)
parseCacheFile path =
  withFile path ReadMode $ \h -> do
    header <- BS.hGet h 4096
    case parseCacheFileHeader header of
      Right !v -> return (Just (CacheEntry (BC.pack path) v))
      Left !_ -> return Nothing

parseCacheFileHeader :: ByteString -> Either Text CacheKey
parseCacheFileHeader bytes =
  let ls = BC.lines bytes
      line = fromMaybe "" $ find (BS.isPrefixOf "KEY: ") ls
  in case A.parseOnly parseCacheKey line of
       Right x -> Right x
       Left e -> Left . toS $ e
