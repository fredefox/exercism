{-# language OverloadedStrings #-}
module Manifest ( Manifest, getTaskId, getManifest ) where

import Data.Aeson ( FromJSON (parseJSON), (.:))
import qualified Data.Aeson as Aeson
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text

newtype Test = Test
  { taskId :: Maybe Int
  }

instance FromJSON Test where
  parseJSON = Aeson.withObject "test" $ \v -> Test <$> v .: "task_id"

newtype Manifest = Manifest
  { _tests :: Map Text Test
  }

instance FromJSON Manifest where
  parseJSON = Aeson.withObject "manifest" $ \v -> Manifest <$> v .: "tests"

-- | Map test descriptions to task IDs based on a manifest.
getTaskId :: Manifest -> String -> Maybe Int
getTaskId (Manifest m) name = Map.lookup (Text.pack name) m >>= taskId

-- TODO!
findManifest :: IO FilePath
findManifest = pure "./.exercism/metadata.json"

getManifest :: IO (Maybe Manifest)
getManifest = findManifest >>= Aeson.decodeFileStrict
