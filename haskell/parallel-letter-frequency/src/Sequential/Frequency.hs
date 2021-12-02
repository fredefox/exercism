module Sequential.Frequency (frequency) where

import Data.Map  (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text

frequency :: [Text] -> Map Char Int
frequency = Map.unionsWith (+) . fmap frequency'

frequency' :: Text -> Map Char Int
frequency' = Map.fromListWith (+) . fmap (\c -> (c, 1)) . Text.unpack

