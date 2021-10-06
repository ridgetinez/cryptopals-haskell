{-# LANGUAGE OverloadedStrings #-}

-- An ECB/CBC detection oracle https://cryptopals.com/sets/2/challenges/11
module Challenge11 where

import System.Random
import Control.Monad.Trans.State
import Control.Monad (replicateM)
import Data.Char (chr)
import Data.ByteString.Char8 (ByteString)

import Challenge07
import Challenge09
import Challenge10
import Crypto.Cipher.AES (AES128)
import Crypto.Cipher.Types (cipherInit)
import Crypto.Error (throwCryptoErrorIO)

import qualified Data.ByteString.Char8 as B
import qualified Data.Set as Set

randomRST :: (Random a, RandomGen g) => (a,a) -> State g a
randomRST range = state (randomR range)

randomST :: (Random a, RandomGen g) => State g a
randomST = state random

randomByteString :: (Int,Int) -> State StdGen ByteString
randomByteString nrange = do
    n <- randomRST nrange 
    s <- replicateM n $ randomRST ('a','z')
    return $ B.pack s

c11 :: IO ()
c11 = do
    (key,g) <- getStdGen >>= \g -> return $ runState (randomByteString (16,16)) g
    cipher  <- throwCryptoErrorIO $ cipherInit key
    let (ctext,_) = runState (encryptionOracle cipher $ B.replicate 64 'a') g
    if detectECB ctext
        then putStrLn "ECB"
        else putStrLn "CBC"

encryptionOracle :: AES128 -> ByteString -> State StdGen ByteString
encryptionOracle cipher plaintext = do
    plaintextPrepend <- randomByteString (5,10)
    plaintextAppend  <- randomByteString (5,10)
    randomIV         <- randomByteString (16,16)
    encryptWithECB   <- randomST
    let toEncrypt = pkcs7pad 16 $ plaintextPrepend `B.append` plaintext `B.append` plaintextAppend
    let ciphertext = if encryptWithECB 
        then encryptAES128ECB cipher toEncrypt 
        else encryptAES128CBC cipher randomIV toEncrypt 
    return ciphertext

detectECB :: ByteString -> Bool
detectECB ciphertext = go Set.empty ciphertext
    where go seen "" = False
          go seen bs = if Set.member chunk seen
                then True 
                else go next $ B.drop blockSize bs
            where chunk = B.take blockSize bs
                  next = Set.insert chunk seen
                  blockSize = 16


