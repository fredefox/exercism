module Roman (numerals) where

import qualified Data.List as List
import Control.Monad

numerals :: Integral n => Read n => n -> Maybe String
numerals n
  | n <= 0 || n >= 4000 = Nothing
  | otherwise           = pure $ join $ solve n

solve :: Integral n => Read n => n -> [String]
solve n = case List.find ((<= n) . snd) table of
  Nothing -> []
  Just (symb, v) -> symb : solve (n - v)

table :: forall n . Integral n => Read n =>[(String, n)]
table = fmap (\case { (a:b:_) -> (a, read @n b) ; _ -> undefined }) $ List.transpose $ fmap words
  [ "M     CM   D  CD   C XC  L XL  X IX V IV I"
  , "1000 900 500 400 100 90 50 40 10  9 5  4 1"
  ]
