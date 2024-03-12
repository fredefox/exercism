module Spiral (spiral) where

spiral :: Int -> [[Int]]
spiral size = error "You need to implement this function."

diameter :: Int -> Int
diameter 0 = 1
diameter r = (4 * width r) - 4

width :: Int -> Int
width r = succ $ 2 * r

size :: Int -> Int
size k = (2 * k + 1) ^ (2 ::Int)

sizeInv :: Int -> Int
sizeInv n = floor $ pred (sqrt @Double $ fromIntegral n) / 2

polar :: Int -> (Int, Int)
polar 0 = (0, 0)
polar n = (r, theta)
  where
  r  = succ $ sizeInv n
  theta = n - size (pred r)

id' :: Int -> Int
id' n = let (r, theta) = polar n in size (pred r) + theta

-- toCartesian :: (Int, Int) -> (Int, Int)
toCartesian (0, _) = (0, 0)
toCartesian (r, theta') = rot (r, r - w)
  where
  -- side \in [0, 1, 2, 3]
  theta = theta'
  (side, w) = theta `divMod` (2 * r)
  rot (x, y)
    | side == 3 = (x, y)
    | side == 2 = (-y, x)
    | side == 1 = (-x, -y)
    | otherwise = (y, -x)
