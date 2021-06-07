module Asana.Api.Project
  ( Project(..)
  , getProjects
  ) where

import RIO

import Asana.Api.Gid (Gid)
import Asana.Api.Request (HasAsana, getAllParams)
import Data.Aeson (FromJSON, genericParseJSON, parseJSON)
import Data.Aeson.Casing (aesonPrefix, snakeCase)
import qualified RIO.Text as T
import RIO.Time (UTCTime)

data Project = Project
  { pGid :: Gid
  , pName :: Text
  , pCreatedAt :: UTCTime
  }
  deriving stock (Generic, Show)

instance FromJSON Project where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

getProjects
  :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env, HasAsana env)
  => m [Project]
getProjects = getAllParams
  (T.unpack "/projects/")
  [("team", "12760955045995"), ("opt_fields", "created_at,name")]
