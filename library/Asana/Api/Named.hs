-- | Anything with a compact @{ id, name }@ representations
module Asana.Api.Named
    ( Named(..)
    ) where

import RIO

import Data.Aeson
import Data.Aeson.Casing
import RIO.Text (Text)

data Named = Named
  { nId :: Integer
  , nName :: Text
  }
  deriving (Eq, Generic, Show)

instance FromJSON Named where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase
