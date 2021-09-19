{-# LANGUAGE OverloadedStrings #-}

module Challenge06 where

import Data.Char (ord)
import Data.Function (on)
import Data.List (sortBy)
import Data.Either (fromRight)
import Data.Bits (xor, popCount)
import Data.ByteString.Char8 (ByteString)

import Challenge03 (decryptXOR, createFrequencyMap)

import qualified Data.Text.IO as T
import qualified Data.List.Split as S
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Base64 as B64

c6 :: IO ()
c6 = do
    corpus <- T.readFile "data/muchadoaboutnothing.txt"
    let frequencyMap = createFrequencyMap corpus
    ciphertext <- B.readFile "data/c6.txt" 
        >>= return . fromRight B.empty . B64.decode . B.concat . B.split '\n'
    let likelihoods = sortBy (compare `on` snd) $ keyLikelihoods ciphertext
    putStrLn $ show $ likelihoods
    let transposed = transpose $ chunksOf 29 ciphertext
    let keys = map fst $ map (decryptXOR frequencyMap) transposed
    putStrLn $ show keys

transpose :: [ByteString] -> [ByteString]
transpose [] = []
transpose ("":_) = []
transpose bs = B.pack (map B.head bs) : (transpose $ map B.tail bs)

keyLikelihoods :: ByteString -> [(Int,Float)]
keyLikelihoods bs = zip keysizes 
        $ map likelihood 
        $ map (flip chunksOf bs) keysizes
    where keysizes = [1..40]
          likelihood chunks = flip (/) (fromIntegral (length chunks))
            $ sum 
            $ map (uncurry normalisedEditDistance) 
            $ zip chunks (tail chunks)
          
-- list of chunks with size n. If not an even multiple, padded with 'a'.
chunksOf :: Int -> ByteString -> [ByteString]
chunksOf n = map B.pack . map (fill n) . S.chunksOf n . B.unpack
    where fill nelems s = s ++ replicate (nelems - length s) 'a'

normalisedEditDistance :: ByteString -> ByteString -> Float
normalisedEditDistance a b = fromIntegral (hammingDistance a b) / fromIntegral (B.length a)

hammingDistance :: ByteString -> ByteString -> Int 
hammingDistance a b = sum $ B.zipWith charDistance a b
    where charDistance x y = popCount $ xor (ord x) (ord y)