{-# LANGUAGE OverloadedStrings #-}

module Challenge07 where

import Crypto.Cipher.AES (AES128)
import Crypto.Cipher.Types (cipherInit, ecbDecrypt, ecbEncrypt)
import Crypto.Error (throwCryptoErrorIO)
import Data.Either (fromRight)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Base64 as B64

c7 :: IO ()
c7 = do
    corpus <- B.readFile "data/c7.txt" >>= 
        return . fromRight B.empty . B64.decode . B.concat . B.split '\n'
    cipher <- throwCryptoErrorIO $ cipherInit ("YELLOW SUBMARINE" :: ByteString)
    B.putStrLn $ decryptAES128ECB cipher corpus

encryptAES128ECB :: AES128 -> ByteString -> ByteString
encryptAES128ECB cipher bs = ecbEncrypt cipher bs

-- Where does the ecbDecrypt functions come from?
decryptAES128ECB :: AES128 -> ByteString -> ByteString
decryptAES128ECB cipher bs = ecbDecrypt cipher bs