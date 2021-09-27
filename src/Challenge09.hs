{-# LANGUAGE OverloadedStrings #-}

-- Implement PKCS#7 Padding https://cryptopals.com/sets/2/challenges/9
module Challenge09 where

import Data.Char (chr)

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B

-- Pad length <= 256. This is so that the number that is padded to the end 
-- of the bytestring can be represented as a single byte. In the specific
-- where padLen = 256, we know that if we were to pad, then the max number
-- to pad would be 255 bytes, as if it were 256, then the bytestring would
-- be an even multiple of the padlen.
pkcs7pad :: Int -> ByteString -> ByteString
pkcs7pad padLen bs = B.append bs $ B.replicate numToAppend $ chr numToAppend
    where x = padLen - (B.length bs `mod` padLen)
          numToAppend = if x == padLen then 0 else x