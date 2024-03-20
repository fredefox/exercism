{-# language NamedFieldPuns #-}
module DND ( Character(..)
           , ability
           , modifier
           , character
           ) where

import Test.QuickCheck (Gen, Arbitrary)
import qualified Test.QuickCheck as Q
import qualified Data.List as List

data Character = Character
  { strength     :: Int
  , dexterity    :: Int
  , constitution :: Int
  , intelligence :: Int
  , wisdom       :: Int
  , charisma     :: Int
  , hitpoints    :: Int
  }
  deriving (Show, Eq)

instance Arbitrary Character where
  arbitrary = do
    strength <- ability
    dexterity <- ability
    constitution <- ability
    intelligence <- ability
    wisdom <- ability
    charisma <- ability
    pure $ Character
      { strength
      , dexterity
      , constitution
      , intelligence
      , wisdom
      , charisma
      , hitpoints = 10 + modifier constitution
      }
  
modifier :: Int -> Int
modifier n = (n - 10) `div` 2

ability :: Gen Int
ability = do
  a <- dice
  b <- dice
  c <- dice
  d <- dice
  pure $ sum $ take 3 $ List.sort [a,b,c,d]

dice :: Gen Int
dice = (\n -> succ (n `mod` 6)) <$> Q.arbitrary

character :: Gen Character
character = Q.arbitrary
