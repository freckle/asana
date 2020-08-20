-- | A globally unique identifier
module Asana.Api.Gid
    ( Gid
    , gidToText
    , textToGid
    ) where

import RIO

import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import RIO.Text (Text)

newtype Gid = Gid { gidToText :: Text }
  deriving (Eq, Generic, Show)
  deriving newtype (FromJSON, ToJSON, ToJSONKey, FromJSONKey, Hashable)

textToGid :: Text -> Gid
textToGid = Gid
