module OCR (convert) where

import qualified Data.List as List
import Data.Maybe

convert :: String -> String
convert = List.intercalate "," . solve

solve :: String -> [String]
solve xs = fmap line $ ls
  where
  ls = chunksOf 4 $ lines xs

line :: [String] -> String
line l = fmap letter $ List.transpose $ fmap (chunksOf 3) l

letter :: [String] -> Char
letter s = fromMaybe '?' $ lookup s $
  [ [ " _ "
    , "| |"
    , "|_|"
    , "   " ] |> '0'
  , [ "   "
    , "  |"
    , "  |"
    , "   " ] |> '1'
  , [ " _ "
    , " _|"
    , "|_ "
    , "   " ] |> '2'
  , [ " _ "
    , " _|"
    , " _|"
    , "   " ] |> '3'
  , [ "   "
    , "|_|"
    , "  |"
    , "   " ] |> '4'
  , [ " _ "
    , "|_ "
    , " _|"
    , "   " ] |> '5'
  , [ " _ "
    , "|_ "
    , "|_|"
    , "   " ] |> '6'
  , [ " _ "
    , "  |"
    , "  |"
    , "   " ] |> '7'
  , [ " _ "
    , "|_|"
    , "|_|"
    , "   " ] |> '8'
  , [ " _ "
    , "|_|"
    , " _|"
    , "   " ] |> '9'
  ]
  where
  (|>) = (,)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = a : chunksOf n b
  where
  (a, b) = splitAt n xs

ocrRowChars :: (String, String, String) -> [(String, String, String)]
ocrRowChars (r1, r2, r3) = zip3 (chunksOf 3 r1) (chunksOf 3 r2) (chunksOf 3 r3)
