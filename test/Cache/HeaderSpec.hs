{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cache.HeaderSpec
  ( main
  , spec
  )

where

import           Protolude.Lifted

import           Cache.Header
import           Data.Binary
import qualified Data.ByteString as Bytes
import           Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as ShortBytes
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()



instance Arbitrary ShortByteString where
  arbitrary = ShortBytes.pack <$> arbitrary
  shrink xs = ShortBytes.pack <$> shrink (ShortBytes.unpack xs)

instance Arbitrary CacheHeader where
  arbitrary = do
    version <- oneof [pure 3, pure 5]
    validSec <- arbitrary
    updatingSec <-
      if version == 3
        then pure Nothing
        else arbitrary `suchThat` isJust
    errorSec <-
      if version == 3
        then pure Nothing
        else arbitrary `suchThat` isJust
    lastModified <- arbitrary
    date <- arbitrary
    crc32 <- arbitrary
    validMSec <- arbitrary
    bodyStart <- arbitrary
    etagLen <- arbitrary
    let len =
          if version == 5
            then 128
            else 42
    etag <- arbitrary `suchThat` ((== len) . ShortBytes.length)
    varyLen <- arbitrary
    vary <- arbitrary `suchThat` ((== len) . ShortBytes.length)
    variant_ <- arbitrary `suchThat` ((== 16) . ShortBytes.length)
    key <- arbitrary
    let headerStart =
          144 +
          (if version == 5
             then 16 + ((128 - 42) * 2)
             else 0) +
          Bytes.length "\n" +
          Bytes.length "KEY: " +
          Bytes.length key +
          Bytes.length "\n"
    return
      CacheHeader
      { cacheHeaderVersion = version
      , cacheHeaderValidSec = validSec
      , cacheHeaderUpdatingSec = updatingSec
      , cacheHeaderErrorSec = errorSec
      , cacheHeaderLastModified = lastModified
      , cacheHeaderDate = date
      , cacheHeaderCrc32 = crc32
      , cacheHeaderValidMSec = validMSec
      , cacheHeaderHeaderStart = fromIntegral headerStart
      , cacheHeaderBodyStart = bodyStart
      , cacheHeaderEtagLen = etagLen
      , cacheHeaderEtag = etag
      , cacheHeaderVaryLen = varyLen
      , cacheHeaderVary = vary
      , cacheHeaderVariant = variant_
      , cacheHeaderKey = key
      }

main :: IO ()
main = hspec spec

{-# ANN spec ("HLint: ignore Redundant do" :: Text) #-}
spec :: Spec
spec =
  describe "CacheHeader" $ do
    modifyMaxSize (const 200) $
      it "decode . encode == id" $ do
        property $ do
          \(c :: CacheHeader) ->
            let meh = encode c
            in c `shouldBe` decode meh
