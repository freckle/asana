module Asana.Api.Task
  ( Task(..)
  , CustomField(..)
  , Membership(..)
  , TaskStatusFilter(..)
  , ResourceSubtype(..)
  , getTask
  , getProjectTasks
  , getProjectTasksCompletedSince
  , putEnumField
  ) where

import RIO

import Asana.Api.Gid (Gid, gidToText)
import Asana.Api.Named
import Asana.Api.Request
import Asana.App
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Aeson.Casing
import Data.Semigroup ((<>))
import RIO.Text (Text)
import qualified RIO.Text as T
import RIO.Time

-- | Just what we need out of our @custom_fields@ for cost and carry-over
data CustomField
  = CustomNumber Text (Maybe Integer)
  | CustomEnum Text (Maybe Text)
  | Other -- ^ Unexpected types dumped here
  deriving (Eq, Generic, Show)

instance FromJSON CustomField where
  parseJSON = withObject "CustomField" $ \o -> do
    oType <- o .: "type"

    case (oType :: Text) of
      "number" -> CustomNumber <$> o .: "name" <*> o .: "number_value"
      "enum" -> do
        value <- o .: "enum_value"
        CustomEnum <$> o .: "name" <*> case value of
          Object vo -> vo .:? "name"
          _ -> pure Nothing
      _ -> pure Other

-- | We need to know Section to find "Awaiting Deployment"
data Membership = Membership
  { mProject :: Named
  , mSection :: Maybe Named
  }
  deriving (Eq, Generic, Show)

instance FromJSON Membership where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data ResourceSubtype = DefaultTask | Milestone | Section
  deriving (Eq, Generic, Show)

instance FromJSON ResourceSubtype where
  parseJSON =
    genericParseJSON $ defaultOptions { constructorTagModifier = snakeCase }

data Task = Task
  { tAssignee :: Maybe Named
  , tName :: Text
  , tCompleted :: Bool
  , tCompletedAt :: Maybe UTCTime
  , tCustomFields :: [CustomField]
  , tMemberships :: [Membership]
  , tGid :: Gid
  , tResourceSubtype :: ResourceSubtype
  }
  deriving (Eq, Generic, Show)

instance FromJSON Task where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

-- | Return all details for a task by id
getTask :: Gid -> AppM Task
getTask taskId = getSingle $ "/tasks/" <> T.unpack (gidToText taskId)

-- | Return compact task details for a project
--
-- Iterating ourselves and returning @['Task']@ is a better interface but
-- precludes us logging things each time we request an element. So we return
-- @'Named'@ for now and let the caller use @'getTask'@ themselves.
--
getProjectTasks :: Gid -> TaskStatusFilter -> AppM [Named]
getProjectTasks projectId taskStatusFilter = do
  now <- liftIO getCurrentTime
  getAllParams
    (T.unpack $ "/projects/" <> gidToText projectId <> "/tasks")
    (completedSince now)
 where
  completedSince now = case taskStatusFilter of
    AllTasks -> []
    IncompletedTasks -> [("completed_since", formatISO8601 now)]

formatISO8601 :: FormatTime t => t -> String
formatISO8601 = formatTime defaultTimeLocale (iso8601DateFormat Nothing)

data TaskStatusFilter = IncompletedTasks | AllTasks

getProjectTasksCompletedSince :: Gid -> UTCTime -> AppM [Named]
getProjectTasksCompletedSince projectId since = getAllParams
  (T.unpack $ "/projects/" <> gidToText projectId <> "/tasks")
  [("completed_since", formatISO8601 since)]

putEnumField :: Gid -> (Integer, Maybe Integer) -> AppM ()
putEnumField taskId (fieldId, enumId) =
  put ("/tasks/" <> T.unpack (gidToText taskId)) $ object
    [ "data"
        .= object ["custom_fields" .= object [tshow fieldId .= toJSON enumId]]
    ]
