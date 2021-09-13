{-# LANGUAGE OverloadedStrings #-}

module Challenge03 where

import Data.Map (Map)
import Data.Text (Text)
import Data.Word (Word8)
import Challenge02 (fixedXOR, decodeHex)
import Data.ByteString (ByteString)

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.ByteString as B

c3 :: IO ()
c3 = do 
    let frequencyMap = createFrequencyMap 
    let attempts = map (decryptXORWithKey $ decodeHex "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736") [1..255]
    printElements attempts

printElements :: [ByteString] -> IO ()
printElements [] = return ()
printElements s@(x:xs) = do putStr (show $ length s); B.putStr x; putStrLn ""; printElements xs

decryptXORWithKey :: ByteString -> Word8 -> ByteString
decryptXORWithKey bs k = fixedXOR bs $ B.replicate (B.length bs) k

-- Read in an english corpus and create a frequency map
createFrequencyMap :: Text -> Map Char Float
createFrequencyMap corpus = M.map (\a -> a / corpusLength) unnormalisedFreqMap
    where corpusLength = fromIntegral $ T.length corpus
          unnormalisedFreqMap = T.foldl addChar M.empty corpus
          addChar m c = M.insertWithKey (\_ incr old -> old + incr) c 1 m
