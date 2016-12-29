{-# LANGUAGE OverloadedStrings #-}

module Cache.ContentSpec
  ( main
  , spec
  )

where

import           Protolude.Lifted

import           Cache.Content
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.ByteString.Extra
import           Test.Hspec
import           Test.Hspec.Attoparsec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

newtype NormalizedUA =
  NormalizedUA ByteString
  deriving (Show)

instance Arbitrary NormalizedUA where
  arbitrary =
    NormalizedUA <$> arbitrary `suchThat` \bs -> compressChar '-' bs == bs

newtype Protocol =
  Protocol ByteString
  deriving (Show)

instance Arbitrary Protocol where
  arbitrary = Protocol <$> Test.QuickCheck.elements ["https", "http", ""]

main :: IO ()
main = hspec spec

{-# ANN spec ("HLint: ignore Redundant do" :: Text) #-}
spec :: Spec
spec =
  describe "parseCacheKey" $ do
    it "should parse a simple key" $ do
      simpleKey ~> parseCacheKey `shouldParse` simpleCacheKey
    context "assuming normalised UA can't have consecutive '-'" $ do
      modifyMaxSize (const 20000) $
        it "should produce same result regardless of normalised UA" $
        property $ do
          \(NormalizedUA ua) ->
            BS.concat ["KEY: ", ua, noUAKey] ~> parseCacheKey `shouldParse`
            simpleCacheKey
  where
    simpleKey :: ByteString
    simpleKey = "KEY: standard--example.com/path"
    simpleCacheKey :: CacheKey
    simpleCacheKey = CacheKey (DomainName "example.com") "/path"
    noUAKey :: ByteString
    noUAKey = "--example.com/path"
