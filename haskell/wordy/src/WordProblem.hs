module WordProblem (answer) where

import Text.Parsec
import Data.Char

answer :: String -> Maybe Integer
answer s = case runParser parser () mempty s of
  Left{} -> Nothing
  Right a -> pure a

parser :: Parsec String u Integer
parser = do
  _ <- stringw "What is"
  n <- expr
  _ <- stringw "?"
  pure n

stringw :: String -> Parsec String u String
stringw s = spaces *> string s

expr :: Parsec String u Integer
expr = integral `chainl1` ops

ops :: Parsec String u (Integer -> Integer -> Integer)
ops = try plus <|> try minus <|> try mult <|> try divid

plus, minus, mult, divid :: Parsec String u (Integer -> Integer -> Integer)
plus = (+) <$ stringw "plus"
minus = (-) <$ stringw "minus"
mult = (*) <$ stringw "multiplied by"
divid = (div) <$ stringw "divided by"

-- | Uses `read` as a hack.
integral :: Read n => Integral n => Parsec String u n
integral = do
  spaces
  s <- sign
  (s . read) <$> many1 (satisfy isNumber)

sign :: Num a => Parsec String u (a -> a)
sign = do
  m <- optionMaybe $ char '-'
  pure $ case m of
    Nothing -> id
    Just{} -> negate
  
  
