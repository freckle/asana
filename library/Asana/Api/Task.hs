module Asana.Api.Task
  ( Task(..)
  , CustomField(..)
  , CustomFields(..)
  , EnumOption(..)
  , Membership(..)
  , TaskStatusFilter(..)
  , ResourceSubtype(..)
  , ProjectId(..)
  , PostTaskBody(..)
  , getTask
  , getProjectTasks
  , getProjectTasksCompletedSince
  , postTask
  , putCustomField
  , putCustomFields
  , searchProjectByCustomFields
  , taskUrl
  , extractNumberField
  , extractEnumField
  ) where

import RIO

import Asana.Api.Gid (Gid, gidToText)
import Asana.Api.Named (Named)
import Asana.Api.Request (getAllParams, getSingle, post, put)
import Asana.App (AppM)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Aeson.Casing (aesonPrefix, snakeCase)
import Data.List (find)
import Data.Scientific (Scientific)
import Data.Semigroup ((<>))
import qualified RIO.HashMap as HashMap
import RIO.Text (Text)
import qualified RIO.Text as T
import RIO.Time
  ( FormatTime
  , UTCTime
  , defaultTimeLocale
  , formatTime
  , getCurrentTime
  , iso8601DateFormat
  )

newtype ApiData a = ApiData
  { adData :: a
  }
  deriving stock (Generic, Show, Eq)

instance FromJSON a => FromJSON (ApiData a) where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance ToJSON a => ToJSON (ApiData a) where
  toJSON = genericToJSON $ aesonPrefix snakeCase
  toEncoding = genericToEncoding $ aesonPrefix snakeCase

-- | Just what we need out of our @custom_fields@ for cost and carry-over
data CustomField
  = CustomNumber Gid Text (Maybe Scientific)
  | CustomEnum Gid Text [EnumOption] (Maybe Text)
  | CustomText Gid (Maybe Text)
  | Other -- ^ Unexpected types dumped here
  deriving (Eq, Generic, Show)

newtype CustomFields = CustomFields { getCustomFields :: [CustomField] }
  deriving stock (Show, Eq)
  deriving newtype (FromJSON)

instance ToJSON CustomFields where
  toJSON (CustomFields fields) = object $ concatMap toPair fields
   where
    toPair = \case
      CustomNumber gid _ n -> [gidToText gid .= n]
      e@(CustomEnum gid _ _ _) -> [gidToText gid .= customEnumId e]
      _ -> []

data EnumOption = EnumOption
  { eoGid :: Gid
  , eoName :: Text
  }
  deriving (Eq, Generic, Show)

instance FromJSON EnumOption where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

-- | Return a @'CustomField'@s value's Enum id, is possible
--
-- - Must be a @'CustomEnum'@
-- - Must have a value
-- - Must have an option with the same name as that value
--
customEnumId :: CustomField -> Maybe Gid
customEnumId (CustomEnum _ _ opts mValue) = do
  value <- mValue
  option <- find ((== value) . eoName) opts
  pure $ eoGid option
customEnumId _ = Nothing

instance FromJSON CustomField where
  parseJSON = withObject "CustomField" $ \o -> do
    oType <- o .: "type"

    case (oType :: Text) of
      "text" -> CustomText <$> o .: "gid" <*> o .: "text_value"
      "number" ->
        CustomNumber <$> o .: "gid" <*> o .: "name" <*> o .: "number_value"
      "enum" -> do
        value <- o .: "enum_value"
        CustomEnum
          <$> o
          .: "gid"
          <*> o
          .: "name"
          <*> o
          .: "enum_options"
          <*> case value of
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
  , tCreatedAt :: UTCTime
  , tCustomFields :: CustomFields
  , tMemberships :: [Membership]
  , tGid :: Gid
  , tResourceSubtype :: ResourceSubtype
  , tNotes :: Text
  }
  deriving (Eq, Generic, Show)

instance FromJSON Task where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

-- | Return all details for a task by id
getTask :: Gid -> AppM ext Task
getTask taskId = getSingle $ "/tasks/" <> T.unpack (gidToText taskId)

newtype ProjectId = ProjectId { getProjectId :: Text }
  deriving newtype (ToJSON, FromJSON)

data PostTaskBody = PostTaskBody
  { ptbProjects :: [ProjectId]
  , ptbCustomFields :: HashMap Gid Text
  , ptbName :: Text
  , ptbNotes :: Text
  , ptbParent :: Maybe Gid
  }
  deriving Generic

instance FromJSON PostTaskBody where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance ToJSON PostTaskBody where
  toJSON = genericToJSON $ aesonPrefix snakeCase
  toEncoding = genericToEncoding $ aesonPrefix snakeCase

-- | Create a new 'Task'
postTask :: PostTaskBody -> AppM ext (Result (ApiData Task))
postTask body = fromJSON <$> post "/tasks" (ApiData body)

-- | Search for tasks within a workspace & project matching 'CustomField's
searchProjectByCustomFields
  :: Gid -> Gid -> HashMap Gid Text -> AppM ext [Named]
searchProjectByCustomFields workspaceId projectId customFields =
  getAllParams
      (T.unpack $ "/workspaces/" <> gidToText workspaceId <> "/tasks/search")
    $ ("projects.all", T.unpack $ gidToText projectId)
    : customFieldParams
 where
  customFieldParams =
    map
        (\(a, b) ->
          ("custom_fields." <> T.unpack (gidToText a) <> ".value", T.unpack b)
        )
      $ HashMap.toList customFields

-- | Return compact task details for a project
--
-- Iterating ourselves and returning @['Task']@ is a better interface but
-- precludes us logging things each time we request an element. So we return
-- @'Named'@ for now and let the caller use @'getTask'@ themselves.
--
getProjectTasks :: Gid -> TaskStatusFilter -> AppM ext [Named]
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

getProjectTasksCompletedSince :: Gid -> UTCTime -> AppM ext [Named]
getProjectTasksCompletedSince projectId since = getAllParams
  (T.unpack $ "/projects/" <> gidToText projectId <> "/tasks")
  [("completed_since", formatISO8601 since)]

putCustomField :: Gid -> CustomField -> AppM ext ()
putCustomField taskId = putCustomFields taskId . CustomFields . pure

putCustomFields :: Gid -> CustomFields -> AppM ext ()
putCustomFields taskId fields =
  void $ put ("/tasks/" <> T.unpack (gidToText taskId)) $ ApiData
    (object ["custom_fields" .= fields])

taskUrl :: Task -> Text
taskUrl Task {..} = "https://app.asana.com/0/0/" <> gidToText tGid <> "/f"

extractNumberField :: Text -> Task -> Maybe CustomField
extractNumberField fieldName Task {..} =
  listToMaybe $ flip mapMaybe (getCustomFields tCustomFields) $ \case
    customField@(CustomNumber _ t _) -> customField <$ guard (t == fieldName)
    _ -> Nothing

extractEnumField :: Text -> Task -> Maybe CustomField
extractEnumField fieldName Task {..} =
  listToMaybe $ flip mapMaybe (getCustomFields tCustomFields) $ \case
    customField@(CustomEnum _ t _ _) ->
      if t == fieldName then Just customField else Nothing
    _ -> Nothing
