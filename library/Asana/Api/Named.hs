-- | Anything with a compact @{ id, name }@ representations
module Asana.Api.Named
    ( Named(..)
    ) where

import Prelude

import Data.Aeson
import Data.Aeson.Casing
import Data.Text (Text)
import GHC.Generics

data Named = Named
  { nId :: Integer
  , nName :: Text
  }
  deriving (Eq, Generic, Show)

instance FromJSON Named where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase
