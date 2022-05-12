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
getProjects = mconcat <$> traverse
  getProjectsForTeam
  [engineeringTeam, studentTeam, educatorTeam, platformTeam]
 where
  engineeringTeam = "12760955045995"
  studentTeam = "1201325186562301"
  educatorTeam = "1201325186562310"
  platformTeam = "1200567751522718"

getProjectsForTeam
  :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env, HasAsana env)
  => String
  -> m [Project]
getProjectsForTeam team = getAllParams
  (T.unpack "/projects/")
  [("team", team), ("opt_fields", "created_at,name")]
