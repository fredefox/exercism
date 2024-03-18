module PigLatin (translate) where

import Data.Bifunctor
import Data.List (isPrefixOf)

translate :: String -> String
translate = unwords . fmap tr . words

tr :: String -> String
tr xs = b <> a <> "ay"
  where
  (a, b) = consonantPrefix xs

consonantPrefix :: String -> (String, String)
consonantPrefix xs
  | "yt" `isPrefixOf` xs || "xr" `isPrefixOf` xs = (mempty, xs)
  | otherwise = bimap (fmap fst) (fmap fst) $ span go $ zip xs (' ':xs)
  where
  go (a, b) = (consonant a || b == 'q' && a == 'u') && (a /= 'y' || not (consonant b))

consonant :: Char -> Bool
consonant = (`elem` "bcdfghjklmnpqrstvwxyz")
