{-# LANGUAGE OverloadedStrings #-}

module Challenge03 where

import Data.Map (Map)
import Data.List (maximumBy)
import Data.Function (on)
import Data.Char (chr, toLower)
import Data.Text (Text)
import Data.Word (Word8)
import Challenge02 (fixedXOR, decodeHex)
import Data.ByteString.Char8 (ByteString)

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString.Char8 as B

-- Character frequency map, built from a large corpus.
type FreqMap = Map Char Float

c3 :: IO ()
c3 = do 
    corpus <- T.readFile "data/muchadoaboutnothing.txt"
    let frequencyMap = createFrequencyMap corpus
    B.putStrLn $ decryptXOR frequencyMap (decodeHex "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736")

decryptXOR :: FreqMap -> ByteString -> ByteString
decryptXOR f s = maximumBy (compare `on` scoreText f) (decryptXORAllKeys s)

decryptXORAllKeys :: ByteString -> [ByteString]
decryptXORAllKeys ciphertxt = map (applyXOR ciphertxt) keys
    where keys = map chr [1..255]
          applyXOR bs k = fixedXOR bs $ B.replicate (B.length bs) k

-- Score of a text is the sum of each character's frequency value.
-- Text that has the best score will be the maximum, with the heuristic that 
-- characters that appear more often in the corpus will have a higher weight.
scoreText :: FreqMap -> ByteString -> Float
scoreText freqMap = sum . map (\c -> M.findWithDefault 0 c freqMap) . map toLower . B.unpack

-- Read in an english corpus and create a frequency map
createFrequencyMap :: Text -> FreqMap
createFrequencyMap corpus = M.map (\a -> a / corpusLength) unnormalisedFreqMap
    where corpusLength = fromIntegral $ T.length corpus
          unnormalisedFreqMap = T.foldl addChar M.empty corpus
          addChar m c = M.insertWithKey (\_ incr old -> old + incr) (toLower c) 1 m
