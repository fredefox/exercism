{-# language TypeApplications #-}
{-# language LambdaCase #-}
{-# language ViewPatterns #-}
{-# language MultiWayIf #-}
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
import qualified Sequential
import Control.Concurrent

frequency :: Int -> [Text] -> Map Char Int
frequency n xs = Map.unionsWith (+) l
  where
  l :: [Map Char Int]
  l = fmap freq xs `using` parListChunk (length xs `div` n) rdeepseq
-- frequency _ = Sequential.frequency

freq :: Text -> Map Char Int
freq = Map.fromListWith (+) . fmap (\c -> (toLower c, 1)) . filter isAlpha . Text.unpack

main :: IO ()
main = do
  n <- getArgs >>= \case
    (n':_) -> pure $ read @Int n'
    [] -> Control.Concurrent.getNumCapabilities
  let go l = Text.chunksOf (max 1 $ Text.length l `div` n) l
  xs <- go <$> Text.getContents
  traverse_ print $ Map.toList $ if
    | n > 1     -> frequency n xs
    | otherwise -> Sequential.frequency xs
