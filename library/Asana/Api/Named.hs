-- | Anything with a compact @{ id, name }@ representations
module Asana.Api.Named
    ( Named(..)
    ) where

import RIO

import Asana.Api.Generic (GenericAsanaTask)
import Asana.Api.Gid (Gid)
import Data.Aeson (FromJSON, genericParseJSON, parseJSON)
import Data.Aeson.Casing (aesonPrefix, snakeCase)
import RIO.Text (Text)

data Named = Named
  { nGid :: Gid
  , nName :: Text
  }
  deriving (Eq, Generic, Show)

instance FromJSON Named where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance GenericAsanaTask Named
