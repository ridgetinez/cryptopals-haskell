{-# LANGUAGE OverloadedStrings #-}

module Challenge04 where

import Challenge02 (decodeHex)
import Challenge03 (createFrequencyMap, scoreText, decryptXOR)

import Data.List (maximumBy)
import Data.Function (on)

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString.Char8 as B

c4 :: IO ()
c4 = do 
    corpus <- T.readFile "data/muchadoaboutnothing.txt"
    let frequencyMap = createFrequencyMap corpus
    ctexts <- B.readFile "data/c4.txt" >>= \s -> return $ B.lines s
    let solutions = map (decryptXOR frequencyMap . decodeHex) ctexts
    B.putStrLn $ maximumBy (compare `on` scoreText frequencyMap) solutions