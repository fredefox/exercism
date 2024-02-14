{-# language LambdaCase, TypeApplications #-}
module RunLength (decode, encode) where

import qualified Data.List

decode :: String -> String
decode = foldMap f . go
  where
  f (c, n) = replicate n c

go :: String -> [(Char, Int)]
go = Data.List.unfoldr f
  where
  f :: String -> Maybe ((Char, Int), String)
  f = \case
     [] -> Nothing
     s@(c:s') -> case reads' @Int s of
       (n, (c':s'')):_ -> Just ((c', n), s'')
       _ -> Just ((c, 1), s')

reads' :: Read a => ReadS a
reads' (' ':_) = []
reads' xs = reads xs

encode :: String -> String
encode = foldMap f . Data.List.group
  where
  f [] = error "IMPOSSIBLE"
  f xs@(c:_)
    | n == 1    = [c]
    | otherwise = show n <> [c]
    where
    n = length xs
