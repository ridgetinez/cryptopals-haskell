{-# LANGUAGE OverloadedStrings #-}

module Challenge05 where

import Challenge02 (fixedXOR, encodeHex)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B

c5 :: IO ()
c5 = do
    plaintext <- B.readFile "data/c5.txt"
    let key = "ICE"
    B.putStrLn $ encodeHex $ encryptRepeatingKeyXOR key plaintext

-- Repeat the provided string until we've reached the length given.
strictCycle :: Int -> ByteString -> ByteString
strictCycle n bs
    | n <= B.length bs = B.take n bs
    | otherwise        = B.append bs $ strictCycle (n - B.length bs) bs

encryptRepeatingKeyXOR :: ByteString -> ByteString -> ByteString
encryptRepeatingKeyXOR key plaintext = fixedXOR plaintext $ strictCycle (B.length plaintext) key 
