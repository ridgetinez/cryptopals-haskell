{-# LANGUAGE OverloadedStrings #-}

-- Implement CBC mode https://cryptopals.com/sets/2/challenges/10
module Challenge10 where

import Test.QuickCheck
import Challenge02 (fixedXOR)
import Challenge07 (decryptAES128ECB, encryptAES128ECB)
import Challenge09 (pkcs7pad)
import Data.Char (chr)
import Data.Either (fromRight)
import Data.ByteString.Char8 (ByteString)

import Crypto.Cipher.AES (AES128)
import Crypto.Cipher.Types (cipherInit)
import Crypto.Error (throwCryptoErrorIO)

import qualified Data.List.Split as S
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Base64 as B64

blockSize :: Int
blockSize = 16

c10 :: IO ()
c10 = do
    ctext <- B.readFile "data/c10.txt" >>= 
        return . fromRight B.empty . B64.decode . B.concat . B.split '\n'
    cipher <- throwCryptoErrorIO $ cipherInit ("YELLOW SUBMARINE" :: ByteString)
    B.putStrLn $ decryptAES128CBC cipher (B.replicate blockSize $ chr 0) ctext

aes128CBCProperties :: IO ()
aes128CBCProperties = do
    cipher <- throwCryptoErrorIO $ cipherInit ("YELLOW SUBMARINE" :: ByteString)
    let iv = B.replicate blockSize $ chr 0
    quickCheck $ prop_aes128CBC_inverse cipher iv

-- Strings that are a multiple of blockSize when encrypted then decrypted, will be as if we mapped the identity function.
prop_aes128CBC_inverse :: AES128 -> ByteString -> String -> Bool
prop_aes128CBC_inverse cipher iv x = (decryptAES128CBC cipher iv $ encryptAES128CBC cipher iv plaintext) == plaintext
    where plaintext = pkcs7pad blockSize $ B.pack x

decryptAES128CBC :: AES128 -> ByteString -> ByteString -> ByteString
decryptAES128CBC _ _ "" = ""
decryptAES128CBC cipher iv bs = B.concat [combined, decryptAES128CBC cipher chunk $ B.drop blockSize bs]
    where combined  = fixedXOR iv decrypted
          chunk = B.take blockSize bs
          decrypted = decryptAES128ECB cipher chunk

encryptAES128CBC :: AES128 -> ByteString -> ByteString -> ByteString
encryptAES128CBC _ _ "" = ""
encryptAES128CBC cipher iv bs = B.concat [encrypted, encryptAES128CBC cipher encrypted $ B.drop blockSize bs]
    where chunk = fixedXOR iv $ B.take blockSize bs
          encrypted = encryptAES128ECB cipher chunk