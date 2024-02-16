{-# language LambdaCase #-}
{-# language MultiWayIf #-}
module Knapsack (maximumValue) where

maximumValue :: Int -> [(Int, Int)] -> Int
maximumValue wt = \case
  [] -> 0
  ((w, v):xs) -> if
    | wt >= w -> incl `max` noincl
    | otherwise -> noincl
    where
    incl = v + maximumValue (wt - w) xs
    noincl = maximumValue wt xs
  
