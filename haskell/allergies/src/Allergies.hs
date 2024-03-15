module Allergies (Allergen(..), allergies, isAllergicTo) where

import Data.Bits

data Allergen = Eggs
              | Peanuts
              | Shellfish
              | Strawberries
              | Tomatoes
              | Chocolate
              | Pollen
              | Cats
              deriving (Eq, Show, Enum)

allergies :: Int -> [Allergen]
allergies n = fmap fst $ filter snd $ zip [Eggs ..] $ testBit n <$> [0..7]

isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo x n = x `elem` allergies n
