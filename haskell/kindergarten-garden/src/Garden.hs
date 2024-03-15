{-# language LambdaCase #-}
module Garden
    ( Plant (..)
    , garden
    , lookupPlants
    ) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

data Plant = Clover
           | Grass
           | Radishes
           | Violets
           deriving (Eq, Show)

type Garden = Map String [Plant]

fromChar :: Char -> Plant
fromChar = \case
  'C' -> Clover
  'G' -> Grass
  'R' -> Radishes
  _   -> Violets

garden :: [String] -> String -> Garden
garden students xs = Map.unionsWith (<>) $ fmap go (lines xs)
  where
  go x = Map.fromList $ zip students $ chunksOf 2 $ fmap fromChar x

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = a : chunksOf n b
  where
  (a, b) = splitAt n xs

lookupPlants :: String -> Garden -> [Plant]
lookupPlants k m = fromMaybe mempty $ Map.lookup k m
