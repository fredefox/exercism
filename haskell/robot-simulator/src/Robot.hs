{-# language DerivingStrategies, StandaloneDeriving #-}
module Robot
    ( Bearing(East,North,South,West)
    , bearing
    , coordinates
    , mkRobot
    , move
    ) where

data Bearing = North
             | East
             | South
             | West
             deriving (Eq, Show)

deriving stock instance Enum Bearing
deriving stock instance Bounded Bearing

type Pos = (Integer, Integer)

data Robot = Robot { bearing :: Bearing, coordinates :: Pos }

mkRobot :: Bearing -> Pos -> Robot
mkRobot = Robot

move :: Robot -> String -> Robot
move robot instructions = foldl (flip step) robot instructions

pred', succ' :: Bearing -> Bearing
pred' n
  | n == minBound = maxBound
  | otherwise = pred n

succ' n
  | n == maxBound = minBound
  | otherwise = succ n

step :: Char -> Robot -> Robot
step c = case c of
  'R' -> turn succ'
  'L' -> turn pred'
  _   -> advance

plus :: Pos -> Pos -> Pos
(a0, a1) `plus` (b0, b1) = (a0 + b0, a1 + b1)

advance :: Robot -> Robot
advance (Robot b c) = Robot b $ c `plus` d
  where
  d = case b of
    North -> (0, 1)
    South -> (0, -1)
    East  -> (1, 0)
    West  -> (-1, 0)

turn :: (Bearing -> Bearing) -> Robot -> Robot
turn f (Robot b c) = Robot (f b) c
