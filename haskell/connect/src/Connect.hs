module Connect (Mark(..), winner) where

import Data.Char

data Mark = Cross | Nought deriving (Eq, Show)

-- . . . . .
--  . . . . .
--   . . . . .
--    . . . . .
--     . . . . .
-- ->
-- .XXXX
-- O....
-- O....
-- O....
-- O....
normalize :: [String] -> [String]
normalize = fmap (filter (not . isSpace))
-- normalize = fmap (snd . foldr go (True, [])) . zipWith drop [0..]
--   where
--   go c (b, acc) = (not b, if b then c : acc else acc)

winner :: [String] -> Maybe Mark
winner board = error "You need to implement this function."
