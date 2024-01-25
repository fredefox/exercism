module Sequential (frequency) where

import Data.Map  (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Char

frequency :: [Text] -> Map Char Int
frequency = Map.unionsWith (+) . fmap frequency'

frequency' :: Text -> Map Char Int
frequency' = Map.fromListWith (+) . fmap (\c -> (toLower c, 1)) . filter isAlpha . Text.unpack

