{-# LANGUAGE ViewPatterns #-}

module Data.ByteString.Extra
(
    compress
    , compressWord8
    , compressChar
)

where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char
import Data.Word

compress :: ByteString -> ByteString
compress (BS.uncons -> Nothing) = BS.empty
compress (BS.uncons -> Just (x,BS.uncons -> Nothing)) = BS.singleton x
compress (BS.uncons -> Just (x,BS.uncons -> Just (y,xs)))
    | x == y = compress $ BS.cons y xs
    | otherwise = BS.cons x (compress $ BS.cons y xs)
compress _ = error "Data.ByteString.Extra.compress unhandled case"


compressWord8 :: Word8 -> ByteString -> ByteString
compressWord8 _ (BS.uncons -> Nothing) = BS.empty
compressWord8 _ (BS.uncons -> Just (x,BS.uncons -> Nothing)) = BS.singleton x
compressWord8 c (BS.uncons -> Just (x,BS.uncons -> Just (y,xs)))
    | x == c && x == y = compressWord8 c $ BS.cons y xs
    | otherwise = BS.cons x (compressWord8 c $ BS.cons y xs)
compressWord8 _ _ = error "Data.ByteString.Extra.compressWord8 unhandled case"

compressChar :: Char -> ByteString -> ByteString
compressChar c = compressWord8 (fromIntegral (ord c))
