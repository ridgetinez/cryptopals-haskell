{-# LANGUAGE OverloadedStrings #-}

-- Byte at a time ECB decryption (no prepended random string)
-- https://cryptopals.com/sets/2/challenges/12
module Challenge12 where

import Challenge07
import Challenge09

import Data.Either (fromRight)
import Data.ByteString.Char8 (ByteString)

import Crypto.Cipher.AES (AES128)
import Crypto.Cipher.Types (cipherInit)
import Crypto.Error (throwCryptoErrorIO)

import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as B

c12 :: IO ()
c12 = do
    cipher  <- throwCryptoErrorIO $ cipherInit ("YELLOW SUBMARINE" :: ByteString)
    let encryptFunc = dummyEncryptAES128ECB cipher
    B.putStrLn $ encryptFunc "aaaaaaaaaaaaaaaa"

decryptSecret :: (ByteString -> ByteString) -> ByteString
decryptSecret secretEncrypt = ""

decryptBlock :: (ByteString -> ByteString) -> ByteString
decryptBlock secretEncrypt = 
    where go seen nbyte suffix = [] -- plan this out nicer! interesting problem of enumerating possibilities
          payload byte nbyte suffix = B.replicate 'A' nbyte `B.append` B.pack byte `B.append` suffix

dummyEncryptAES128ECB :: AES128 -> ByteString -> ByteString
dummyEncryptAES128ECB cipher input = encryptAES128ECB cipher (pkcs7pad 16 $ input `B.append` secret)
    where secret = fromRight B.empty . B64.decode $ "Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkgaGFpciBjYW4gYmxvdwpUaGUgZ2lybGllcyBvbiBzdGFuZGJ5IHdhdmluZyBqdXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUgYnkK"
          