module Asana.Api.Task
  ( Task(..)
  , CustomField(..)
  , CustomFields(..)
  , EnumOption(..)
  , Membership(..)
  , TaskStatusFilter(..)
  , TaskTypeFilter(..)
  , ResourceSubtype(..)
  , PostTask(..)
  , SearchWorkspace(..)
  , getTask
  , getProjectTasks
  , getProjectTasksCompletedSince
  , postTask
  , putCustomField
  , putCustomFields
  , putCompleted
  , searchWorkspace
  , taskUrl
  , extractNumberField
  , extractEnumField
  ) where

import RIO

import Asana.Api.Gid (AsanaReference(..), Gid, gidToText)
import Asana.Api.Named (Named)
import Asana.Api.Request (HasAsana, getAllParams, getSingle, post, put)
import Data.Aeson
import Data.Aeson.Casing (aesonPrefix, snakeCase)
import Data.List (find, intercalate)
import Data.Scientific (Scientific)
import qualified RIO.HashMap as HashMap
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
  deriving newtype (Show, Eq)
  deriving stock Generic

instance FromJSON a => FromJSON (ApiData a) where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance ToJSON a => ToJSON (ApiData a) where
  toJSON = genericToJSON $ aesonPrefix snakeCase
  toEncoding = genericToEncoding $ aesonPrefix snakeCase

-- | Just what we need out of our @custom_fields@ for cost and carry-over
data CustomField
  = CustomNumber Gid Text (Maybe Scientific)
  | CustomEnum Gid Text [EnumOption] (Maybe Text)
  | CustomText Gid Text (Maybe Text)
  | Other -- ^ Unexpected types dumped here
  deriving stock (Eq, Generic, Show)

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
  deriving stock (Eq, Generic, Show)

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
      "text" -> CustomText <$> o .: "gid" <*> o .: "name" <*> o .: "text_value"
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

data Membership = Membership
  { mProject :: Named
  , mSection :: Maybe Named
  }
  deriving stock (Eq, Generic, Show)

instance FromJSON Membership where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data ResourceSubtype = DefaultTask | Milestone | Section
  deriving stock (Eq, Generic, Show)

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
  , tProjects :: [AsanaReference]
  }
  deriving stock (Eq, Generic, Show)

instance FromJSON Task where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

-- | Return all details for a task by id
getTask
  :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env, HasAsana env)
  => Gid
  -> m Task
getTask taskId = getSingle $ "/tasks/" <> T.unpack (gidToText taskId)

data PostTask = PostTask
  { ptProjects :: [Gid]
  , ptCustomFields :: HashMap Gid Text
  , ptName :: Text
  , ptNotes :: Text
  , ptParent :: Maybe Gid
  }
  deriving stock Generic

instance FromJSON PostTask where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance ToJSON PostTask where
  toJSON = genericToJSON $ aesonPrefix snakeCase
  toEncoding = genericToEncoding $ aesonPrefix snakeCase

-- | Create a new 'Task'
postTask
  :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env, HasAsana env)
  => PostTask
  -> m (Result Task)
postTask body = fmap adData . fromJSON <$> post "/tasks" (ApiData body)

data TaskTypeFilter = TasksOnly | SubtasksOnly | AllTaskTypes

data SearchWorkspace = SearchWorkspace
  { swWorkspaceId :: Gid
  , swProjectIds :: [Gid]
  , swTaskStatusFilter :: TaskStatusFilter
  , swCustomFields :: HashMap Gid Text
  , swTaskTypeFilter :: TaskTypeFilter
  }

-- | Search for tasks within a workspace
searchWorkspace
  :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env, HasAsana env)
  => SearchWorkspace
  -> m [Named]
searchWorkspace SearchWorkspace {..} =
  getAllParams
      (T.unpack $ "/workspaces/" <> gidToText swWorkspaceId <> "/tasks/search")
    $ ( "projects.all"
      , intercalate "," $ map (T.unpack . gidToText) swProjectIds
      )
    : customFieldParams
    <> completed
    <> isSubtask
 where
  customFieldParams =
    map
        (\(a, b) ->
          ("custom_fields." <> T.unpack (gidToText a) <> ".value", T.unpack b)
        )
      $ HashMap.toList swCustomFields

  completed = case swTaskStatusFilter of
    AllTasks -> []
    IncompletedTasks -> [("completed", "false")]

  isSubtask = case swTaskTypeFilter of
    AllTaskTypes -> []
    TasksOnly -> [("is_subtask", "false")]
    SubtasksOnly -> [("is_subtask", "true")]

-- | Return compact task details for a project
--
-- Iterating ourselves and returning @['Task']@ is a better interface but
-- precludes us logging things each time we request an element. So we return
-- @'Named'@ for now and let the caller use @'getTask'@ themselves.
--
getProjectTasks
  :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env, HasAsana env)
  => Gid
  -> TaskStatusFilter
  -> m [Named]
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

getProjectTasksCompletedSince
  :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env, HasAsana env)
  => Gid
  -> UTCTime
  -> m [Named]
getProjectTasksCompletedSince projectId since = getAllParams
  (T.unpack $ "/projects/" <> gidToText projectId <> "/tasks")
  [("completed_since", formatISO8601 since)]

putCustomField
  :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env, HasAsana env)
  => Gid
  -> CustomField
  -> m ()
putCustomField taskId = putCustomFields taskId . CustomFields . pure

putCustomFields
  :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env, HasAsana env)
  => Gid
  -> CustomFields
  -> m ()
putCustomFields taskId fields =
  void $ put ("/tasks/" <> T.unpack (gidToText taskId)) $ ApiData
    (object ["custom_fields" .= fields])

putCompleted
  :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env, HasAsana env)
  => Gid
  -> Bool
  -> m ()
putCompleted taskId completed =
  void $ put ("/tasks/" <> T.unpack (gidToText taskId)) $ ApiData
    (object ["completed" .= completed])

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
