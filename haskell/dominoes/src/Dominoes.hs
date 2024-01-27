{-# language LambdaCase #-}
{-# options_ghc -Wno-all #-}
module Dominoes (chain) where

import Control.Applicative
import Control.Monad
import qualified Data.List as List
import Data.Maybe (listToMaybe)

import qualified Dom

chain :: [Dom] -> Maybe [Dom]
chain = Dom.chain

type Dom = (Int, Int)

match :: Dom -> Dom -> Bool
(_, a) `match` (b, _) = a == b

solve :: [Dom] -> [[Dom]]
solve xs
  = filter endsMatch
  $ dropWhile (not . subList xs)
  $ takeWhile (`subList` xs)
  $ go match [] xs
  where
  endsMatch = \case
    [] -> True
    xs@(x:_) -> x `match` last xs

xs `subList` ys = List.null $ xs List.\\ ys

valid xs ys = List.sort xs == List.sort ys

go :: Ord a => (a -> a -> Bool) -> [a] -> [a] -> [[a]]
-- Note bene: `prev` is reversed for readability.
go p prev xs = ys <> do
  x <- xs
  case prev of
    [] -> pure ()
    (cur:_) -> guard $ cur `p` x
  go p (x:prev) xs -- $ List.delete x xs
  where
  ys | valid xs prev = pure $ reverse prev
     | otherwise = empty

data Case = Case { description :: String
                 , input       :: [(Int, Int)]
                 , expected    :: Bool
                 }

cases =
  [ Case { description = "empty input = empty output"
         , input       = []
         , expected    = True
         }
  , Case { description = "singleton input = singleton output"
         , input       = [(1, 1)]
         , expected    = True
         }
  , Case { description = "singleton that can't be chained"
         , input       = [(1, 2)]
         , expected    = False
         }
  , Case { description = "three elements"
         , input       = [(1, 2), (3, 1), (2, 3)]
         , expected    = True
         }
  , Case { description = "can reverse dominoes"
         , input       = [(1, 2), (1, 3), (2, 3)]
         , expected    = True
         }
  , Case { description = "cannot use the same domino in both directions"
         , input       = [(1, 2), (2, 3), (2, 1)]
         , expected    = False
         }
  , Case { description = "can't be chained"
         , input       = [(1, 2), (4, 1), (2, 3)]
         , expected    = False
         }
  , Case { description = "disconnected - simple"
         , input       = [(1, 1), (2, 2)]
         , expected    = False
         }
  , Case { description = "disconnected - double loop"
         , input       = [(1, 2), (2, 1), (3, 4), (4, 3)]
         , expected    = False
         }
  , Case { description = "disconnected - single isolated"
         , input       = [(1, 2), (2, 3), (3, 1), (4, 4)]
         , expected    = False
         }
  , Case { description = "need backtrack"
         , input       = [(1, 2), (2, 3), (3, 1), (2, 4), (2, 4)]
         , expected    = True
         }
  , Case { description = "separate loops"
         , input       = [(1, 2), (2, 3), (3, 1), (1, 1), (2, 2), (3, 3)]
         , expected    = True
         }
  , Case { description = "nine elements"
         , input       = [(1, 2), (5, 3), (3, 1), (1, 2), (2, 4), (1, 6), (2, 3), (3, 4), (5, 6)]
         , expected    = True
         }
  , Case { description = "twelve elements - no loop"
         , input       = [(1, 2), (5, 3), (3, 1), (1, 2), (2, 4), (1, 6), (2, 3), (3, 4), (5, 6), (3,6), (4,5), (2,1)]
         , expected    = False
         }
  ]
