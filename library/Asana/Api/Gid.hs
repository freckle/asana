-- | A globally unique identifier
module Asana.Api.Gid
    ( Gid
    , AsanaReference(..)
    , gidToText
    , textToGid
    ) where

import RIO

import Data.Aeson
  (FromJSON(..), FromJSONKey, ToJSON, ToJSONKey, genericParseJSON)
import Data.Aeson.Casing (aesonPrefix, snakeCase)
import RIO.Text (Text)

newtype Gid = Gid { gidToText :: Text }
  deriving stock (Eq, Generic, Show)
  deriving newtype (FromJSON, ToJSON, ToJSONKey, FromJSONKey, Hashable)

newtype AsanaReference = AsanaReference { arGid :: Gid }
  deriving stock (Eq, Generic, Show)

instance FromJSON AsanaReference where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

textToGid :: Text -> Gid
textToGid = Gid
