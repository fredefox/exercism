module Queens (boardString, canAttack) where

import Data.Array (Array)
import qualified Data.Array as Array
import Data.List

mkBoard :: ((Int, Int) -> a) -> Array (Int, Int) a
mkBoard f = Array.array bounds $ f' <$> Array.range bounds
  where
  bounds = ((0, 0), (7, 7))
  f' ix = (ix, f ix)

toString :: Array (Int, Int) Char -> String
toString a = unlines $ fmap (intersperse ' ') $ chunksOf (succ n) $ Array.elems a
  where
  (_, (n, _)) = Array.bounds a

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = a : chunksOf n b
  where
  (a, b) = splitAt n xs

boardString :: Maybe (Int, Int) -> Maybe (Int, Int) -> String
boardString white black = toString $ mkBoard f
  where
  blank = const '_'
  f :: (Int, Int) -> Char
  f = replacePoint black 'B' $ replacePoint white 'W' $ blank

replacePoint :: Eq a => Maybe a -> b -> (a -> b) -> (a -> b)
replacePoint Nothing _ f = f
replacePoint (Just x) y f = \x' -> if x' == x then y else f x'

canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack a@(a0, a1) b@(b0, b1) = a0 == b0 || a1 == b1 || diag a b

diag :: (Int, Int) -> (Int, Int) -> Bool
(a0, a1) `diag` (b0, b1) = abs (a0 - b0) == abs (a1 - b1)
