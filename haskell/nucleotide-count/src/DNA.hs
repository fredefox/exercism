{-# LANGUAGE LambdaCase, TupleSections #-}
module DNA (nucleotideCounts, Nucleotide(..)) where

import Data.Map (Map)
import qualified Data.Map as Map

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

fromChar :: Char -> Maybe Nucleotide
fromChar = \case
  'A' -> pure A
  'C' -> pure C
  'G' -> pure G
  'T' -> pure T
  _   -> Nothing

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts = maybe (Left mempty) (pure . Map.fromListWith (+) . fmap (,1)) . traverse fromChar
