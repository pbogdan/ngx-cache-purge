{-# LANGUAGE OverloadedStrings #-}

module Text.Regex.Extra
  (escape)

where

import Protolude.Lifted

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import           Data.ByteString.Search

escape
  :: ByteString -- Text to search within
  -> ByteString -- Returned text with characters escaped
escape = escapeChars metachars

escapeChars
  :: [ByteString] -- meta chars
  -> ByteString -- Input text
  -> ByteString -- Output text
escapeChars ms inputText = foldl' escapeMetaChar inputText ms

escapeMetaChar
  :: ByteString -- input text
  -> ByteString -- Character to search for
  -> ByteString -- Returned text with characters escaped
escapeMetaChar inputText textToSearchFor =
  BL.toStrict $ replace textToSearchFor replacementText inputText
  where
    replacementText = "\\" `BS.append` textToSearchFor

metachars :: [ByteString]
metachars = ["\\", "|", "(", ")", "[", "{", "^", "$", "*", "+", "?", "."]
