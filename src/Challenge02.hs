{-# LANGUAGE OverloadedStrings #-}

module Challenge02 where

import Data.Bits (xor)
import Data.Either (fromRight)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16

decodeHex :: ByteString -> ByteString
decodeHex = fromRight B.empty . B16.decode

fixedXOR :: ByteString -> ByteString -> ByteString
fixedXOR x y = B.pack $ B.zipWith xor x y

c2 :: ByteString
c2 = (decodeHex "1c0111001f010100061a024b53535009181c") `fixedXOR` (decodeHex "686974207468652062756c6c277320657965")