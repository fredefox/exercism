{-# language TypeApplications #-}
module Frequency (main, frequency) where

import Data.Map  (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text (getContents)
import Control.Parallel.Strategies
import Data.Char
import System.Environment
import Data.Foldable

frequency :: Int -> [Text] -> Map Char Int
frequency n xs = Map.unionsWith (+) l
  where
  l :: [Map Char Int]
  l = fmap freq xs `using` parListChunk (length xs `div` n) rdeepseq

freq :: Text -> Map Char Int
freq = Map.fromListWith (+) . fmap (\c -> (toLower c, 1)) . filter isAlpha . Text.unpack

main :: IO ()
main = do
  n <- read @Int . head <$> getArgs
  let go l = Text.chunksOf (max 1 $ Text.length l `div` n) l
  xs <- go <$> Text.getContents
  traverse_ print $ Map.toList $ frequency n xs
