{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -funbox-strict-fields #-}

module Cache.Header
  ( CacheHeader(..)
  , parseCacheHeader
  , parseCacheFileHeader
  )

where

import           Protolude.Lifted hiding (get, put)

import           Control.Monad
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import qualified Data.ByteString as Bytes
import qualified Data.ByteString.Lazy as LazyBytes
import           Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as ShortBytes

data CacheHeader = CacheHeader
  { cacheHeaderVersion :: !Word64
  , cacheHeaderValidSec :: !Int64
  , cacheHeaderLastModified :: !Int64
  , cacheHeaderDate :: !Int64
  , cacheHeaderCrc32 :: !Word32
  , cacheHeaderValidMSec :: !Word16
  , cacheHeaderHeaderStart :: !Word16
  , cacheHeaderBodyStart :: !Word16
  , cacheHeaderEtagLen :: !Word8
  , cacheHeaderEtag :: !ShortByteString
  , cacheHeaderVaryLen :: !Word8
  , cacheHeaderVary :: !ShortByteString
  , cacheHeaderVariant :: !ShortByteString
  , cacheHeaderKey :: !ByteString
  } deriving (Eq, Generic, Ord, Show)

instance Binary CacheHeader where
  get = do
    version <- getWord64host
    case version of
      3 -> do
        validSec <- getInt64host
        lastModified <- getInt64host
        date <- getInt64host
        crc32 <- getWord32host
        validMSec <- getWord16host
        headerStart <- getWord16host
        bodyStart <- getWord16host
        etagLen <- getWord8
        etag <- ShortBytes.pack <$> sequenceA (replicate 42 getWord8)
        varyLen <- getWord8
        vary <- ShortBytes.pack <$> sequenceA (replicate 42 getWord8)
        variant <- ShortBytes.pack <$> sequenceA (replicate 16 getWord8)
        read <- bytesRead
        skip (Bytes.length $ "\n" <> "KEY: ")
        key <-
          Bytes.pack <$>
          sequenceA
            (replicate
               (fromIntegral (headerStart - fromIntegral read - 7))
               getWord8)
        return
          CacheHeader
          { cacheHeaderVersion = version
          , cacheHeaderValidSec = validSec
          , cacheHeaderLastModified = lastModified
          , cacheHeaderDate = date
          , cacheHeaderCrc32 = crc32
          , cacheHeaderValidMSec = validMSec
          , cacheHeaderHeaderStart = headerStart
          , cacheHeaderBodyStart = bodyStart
          , cacheHeaderEtagLen = etagLen
          , cacheHeaderEtag = etag
          , cacheHeaderVaryLen = varyLen
          , cacheHeaderVary = vary
          , cacheHeaderVariant = variant
          , cacheHeaderKey = key
          }
      _ -> fail $ "Unrecognised cache file version: " <> show version
  put CacheHeader {..} = do
    putWord64host cacheHeaderVersion
    putInt64host cacheHeaderValidSec
    putInt64host cacheHeaderLastModified
    putInt64host cacheHeaderDate
    putWord32host cacheHeaderCrc32
    putWord16host cacheHeaderValidMSec
    putWord16host cacheHeaderHeaderStart
    putWord16host cacheHeaderBodyStart
    putWord8 cacheHeaderEtagLen
    sequenceA_ $ map put (pad 42 0 (ShortBytes.unpack cacheHeaderEtag))
    putWord8 cacheHeaderVaryLen
    sequenceA_ $ map put (pad 42 0 (ShortBytes.unpack cacheHeaderVary))
    sequenceA_ $ map put (pad 16 0 (ShortBytes.unpack cacheHeaderVariant))
    put (10 :: Word8)
    sequenceA_ $ map put (Bytes.unpack ("KEY: " <> cacheHeaderKey))
    put (10 :: Word8)

instance NFData CacheHeader where

pad :: Int -> a -> [a] -> [a]
pad n x xs
  | length xs >= n = xs
  | otherwise = xs ++ replicate (n - length xs) x

parseCacheHeader :: ByteString -> Either Text CacheHeader
parseCacheHeader bs =
  case decodeOrFail (LazyBytes.fromStrict bs) of
    Right (!_, !_, !x) -> Right x
    Left (!_, !offset, !err) ->
      Left $ "error parsing at offset " <> show offset <> ": " <> toS err

parseCacheFileHeader :: FilePath -> IO (Either Text CacheHeader)
parseCacheFileHeader path = do
  headerOrErr <- decodeFileOrFail path
  case headerOrErr of
    Right !x -> return $ Right x
    Left (offset, err) ->
      return $
      Left $ "error parsing at offset " <> show offset <> ": " <> toS err