module Liteyear.Frequency (frequency) where

import Data.Map  (Map)
import qualified Data.Map as Map
import Data.Text (Text, foldl')
import Data.Char (toLower, isAlpha)
import Control.Parallel.Strategies (parMap, rdeepseq)

frequency :: Int -> [Text] -> Map Char Int
frequency nWorkers texts = Map.unionsWith (+) parallelMaps
  where parallelMaps  = let chunkSize = length texts `div` nWorkers
                            chunks = chunksOf (maximum [1, chunkSize]) texts in
                        parMap rdeepseq (Map.unionsWith (+) . freq') chunks
        freq'         = map (foldl' addChar Map.empty)
        addChar m c
          | isAlpha c = Map.insertWith (+) (toLower c) 1 m
          | otherwise = m
chunksOf :: Int -> [e] -> [[e]]
chunksOf _ []     = []
chunksOf i ls     = take i ls : chunksOf i (drop i ls)
