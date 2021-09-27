{-# LANGUAGE OverloadedStrings #-}

-- An ECB/CBC detection oracle https://cryptopals.com/sets/2/challenges/11
module Challenge11 where

import System.Random
import Data.Char (chr)
import Data.ByteString.Char8 (ByteString)

import qualified Data.ByteString.Char8 as B
import qualified Data.Set as Set

-- TODO(amartinez): Learn some syntax sugar to thread gen type around.

generateRandByteString :: RandomGen g => g -> Int -> (ByteString, g)
generateRandByteString gen len = foldr genChar (B.empty, gen) [1..len]
    where genChar _ (s,g) = let (c,nextGen) = randomR (0,255) g in (B.cons (chr c) s, nextGen)

detectECB :: ByteString -> Bool
detectECB ciphertext = go Set.empty ciphertext
    where go seen "" = False
          go seen bs = if Set.member chunk seen
                then True 
                else go next $ B.drop blockSize bs
            where chunk = B.take blockSize bs
                  next = Set.insert chunk seen
                  blockSize = 16


