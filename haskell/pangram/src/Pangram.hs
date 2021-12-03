module Pangram (isPangram) where

import Data.List
import Data.Char

isPangram :: String -> Bool
isPangram = (==) alpha . nub . sort . filter (`elem` alpha) . map toLower

alpha :: String
alpha = ['a'..'z']
