{-# language TypeApplications #-}
module Frequency (main, frequency) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Control.Parallel.Strategies
import Data.Char (toLower, isAlpha)
import Data.Foldable (traverse_)
import System.Environment
import Control.DeepSeq
import Control.Exception

frequency :: Int -> [Text] -> Map Char Int
-- frequency _ = Map.unionsWith (+) . fmap freq
frequency n xs = Map.unionsWith (+) l
  where
  l :: [Map Char Int]
  l = fmap freq xs `using` parListChunk (length xs `div` n) rdeepseq

freq :: Text -> Map Char Int
freq = Map.fromListWith (+) . fmap (\c -> (toLower c, 1)) . filter isAlpha . Text.unpack


main :: IO ()
main = do
  n <- read @Int . head <$> getArgs
  inp <- Text.getContents
  _ <- evaluate (deepseq inp)
  let xs = Text.chunksOf (Text.length inp `div` n) inp
  print $ length xs
  traverse_ print $ Map.toList $ frequency n xs
