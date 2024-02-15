{-# language StandaloneDeriving #-}
{-# language DerivingStrategies #-}
{-# language LambdaCase #-}
{-# options_ghc -Wall #-}
module Bowling (score, BowlingError(..), frames, Frame) where


data BowlingError = IncompleteGame
                  | InvalidRoll { rollIndex :: Int, rollValue :: Int }
  deriving (Eq, Show)

score :: [Int] -> Either BowlingError Int
score xs = do
  q <- frames' xs
  pure $ sum $ solve q

-- parse :: [Int] -> Either BowlingError ([Frame], [Int])
-- parse xs = case filter _ $ zip [0..] xs of
--   [] -> pure $ frames xs
--   (x:_) -> Left $ _ x

data Frame = Strike | Spare Int | Open Int Int

deriving stock instance Show Frame

frames' :: [Int] -> Either BowlingError ([Frame], [Int])
frames' xs0 = do
  (a, b') <- go 0 10 xs0 []
  let b = take 2 b'
  case filter (\x -> x < 0 || x > 10) b of
    [] -> pure (reverse a, b)
    (x:_) -> Left $ InvalidRoll (-1) x
  where
  go :: Int -> Int -> [Int] -> [Frame] -> Either BowlingError ([Frame], [Int])
  go _idx 0 xs acc = pure (acc, xs)
  go _idx _n [] _acc = Left IncompleteGame
  go idx _n (x0:_) _acc | x0 < 0 || x0 > 10 = Left $ InvalidRoll idx x0
  go idx n (x0:xs) acc
    | x0 == 10 = go (succ idx) (pred n) xs (Strike : acc)
  go idx n (x0:x1:xs) acc
    | x0 + x1 > 10 = Left $ InvalidRoll (succ idx) x1
    | x0 + x1 == 10 = go (idx + 2) (pred n) xs (Spare x0 : acc)
    | otherwise = go (idx + 2) (pred n) xs (Open x0 x1 : acc)
  go _idx _n [_] _acc = undefined

frames :: [Int] -> ([Frame], [Int])
frames xs0 = (reverse a, b)
  where
  (a, b) = go 10 xs0 []
  go :: Int -> [Int] -> [Frame] -> ([Frame], [Int])
  go 0 xs acc = (acc, xs)
  go _n [] acc = (acc, [])
  go n (x0:xs) acc
    | x0 == 10 = go (pred n) xs (Strike : acc)
  go n (x0:x1:xs) acc
    | x0 + x1 == 10 = go (pred n) xs (Spare x0 : acc)
    | otherwise = go (pred n) xs (Open x0 x1 : acc)
  go _n [_] _acc = undefined

solve :: ([Frame], [Int]) -> [Int]
solve (qs0, fill) = go qs0
  where
  go :: [Frame] -> [Int]
  go = \case
    [] -> []
    (q:qs) -> sc q qs fill : go qs
-- solve ([], _) = []
-- solve ((q:qs), fill) = sc q qs fill : solve qs

sc :: Frame -> [Frame] -> [Int] -> Int
sc q qs fill = case q of
  Strike -> 10 + two qs fill
  Spare _ -> 10 + one qs fill
  Open x0 x1 -> x0 + x1
-- sc q qs = case q of
--   Strike -> 10 + case qs of
--     [] -> error "Is this possible?"
--     (Strike:qss) -> sc Strike qss
--     (Spare _:_) -> 10
--     (Open x0 x1:_) -> x0 + x1
--     (Extra _x0:_) -> error "I don't understand this case"
--   Spare _ -> 10 + case qs of
--     [] -> error "Is this possible?"
--     (Strike:_) -> error "I don't understand this case"
--     (Spare x1:_) -> x1
--     (Open x1 _:_) -> x1
--     (Extra x1:_) -> x1
--   Open x0 x1 -> x0 + x1
--   Extra{} -> 0
  
two :: [Frame] -> [Int] -> Int
two [] fill = sum $ take 2 fill
two (q:qs) fill = case q of
  Strike -> 10 + one qs fill
  Spare{} -> 10
  Open x0 x1 -> x0 + x1

one :: [Frame] -> [Int] -> Int
one [] fill = head fill
one (q:_) _ = case q of
  Strike -> 10
  Spare x -> x
  Open x _ -> x
