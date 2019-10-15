-- | A globally unique identifier
module Asana.Api.Gid
    ( Gid
    , gidToText
    ) where

import RIO

import Data.Aeson
import RIO.Text (Text)

newtype Gid = Gid { gidToText :: Text }
  deriving (Eq, Generic, Show)
  deriving newtype (FromJSON, ToJSON)

