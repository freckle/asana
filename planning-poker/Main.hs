module Main (main) where

import Asana.Prelude hiding ((.=))

import Asana.Api.CustomField
import Asana.Api.Gid
import Asana.Api.Named
import Asana.Api.Request
import Asana.Api.Task
import Asana.App
import qualified Asana.Prelude as Logging ((.=))
import Control.Applicative ((<|>))
import qualified Data.ByteString.Lazy as BSL
import Data.Csv
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.String (fromString)
import qualified Data.Text
import qualified Data.Text as T
import UnliftIO.Async (pooledForConcurrentlyN)

data AppExt = AppExt
  { appProjectId :: Gid
  , appTeamProjectId :: Gid
  , appImport :: Maybe FilePath
  }

main :: IO ()
main = do
  app <-
    loadAppWith
    $ AppExt
    <$> parseProjectId
    <*> parseTeamProjectId
    <*> parseImport
  runApp app $ do
    AppExt {..} <- asks appExt
    case appImport of
      Nothing -> exportProjectTasks appProjectId appTeamProjectId
      Just file -> importProjectTasksFromFile file appTeamProjectId

exportProjectTasks :: Gid -> Gid -> AppM AppExt ()
exportProjectTasks projectId teamProjectId = do
  planningPokerTasks <- do
    taskIds <- getProjectTasks projectId AllTasks
    allTasks <- pooledForConcurrentlyN maxRequests taskIds (getTask . nGid)
    pure $ flip mapMaybe allTasks $ \t@Task {..} -> toPlanningPokerTask t
      <$ guard (AsanaReference teamProjectId `elem` tProjects)

  liftIO
    . BSL.writeFile "planning-poker-export.csv"
    $ encodeDefaultOrderedByName planningPokerTasks

  for_ planningPokerTasks $ \task -> logInfo $ planningPokerTaskLog
    summary
    task
    "exported to planning-poker-export.csv"

importProjectTasksFromFile :: FilePath -> Gid -> AppM AppExt ()
importProjectTasksFromFile file projectId = do
  contents <- liftIO $ BSL.readFile file
  case decodeByName contents of
    Left err -> logError $ fromString err
    Right (_, tasks) -> do
      projectTaskGids <- map nGid <$> getProjectTasks projectId AllTasks
      projectTasks <- pooledForConcurrentlyN maxRequests projectTaskGids getTask
      let projectTaskMap = HashMap.fromList $ map (tGid &&& id) projectTasks

      void $ pooledForConcurrentlyN
        maxRequests
        tasks
        (updatePlanningPokerTaskCost projectTaskMap)

updatePlanningPokerTaskCost
  :: HashMap Gid Task -> PlanningPokerTask -> AppM AppExt ()
updatePlanningPokerTaskCost projectTaskMap task = case storyPoints task of
  Nothing -> logInfo $ planningPokerTaskLog
    summary
    task
    "Does not have a cost. Skipping import."
  Just cost -> case HashMap.lookup (issueKey task) projectTaskMap of
    Nothing -> logWarn $ planningPokerTaskLog
      summary
      task
      "Not found in project. Skipping import."

    Just asanaTask@Task {..} -> case extractNumberField "cost" asanaTask of
      Just (CustomNumber costFieldGid _ _) -> do
        putCustomField tGid
          $ CustomNumber costFieldGid "cost" (Just $ fromIntegral cost)

        logInfo
          $ planningPokerTaskLog summary task
          $ "cost was updated to "
          <> T.pack (show cost)

      _ -> logWarn $ planningPokerTaskLog
        summary
        task
        "No 'cost' field.  Skipping import."

planningPokerTaskLog
  :: (PlanningPokerTask -> Text) -> PlanningPokerTask -> T.Text -> Message
planningPokerTaskLog format task message =
  message
    :# [ "task" Logging..= format task
       , "url" Logging..= planningPokerTaskUrl task
       ]

data PlanningPokerTask = PlanningPokerTask
  { issueKey :: Gid
  , summary :: Text
  , description :: Text
  , acceptanceCriteria :: Text
  , storyPoints :: Maybe Integer
  }
  deriving stock Show

instance FromNamedRecord PlanningPokerTask where
  parseNamedRecord m =
    PlanningPokerTask
      <$> (textToGid <$> m .: "Issue Key")
      <*> m
      .: "Summary"
      <*> m
      .: "Description"
      <*> m
      .: "Acceptance Criteria"
      <*> (m .: "Story Points" <|> pure Nothing)

instance ToNamedRecord PlanningPokerTask where
  toNamedRecord PlanningPokerTask {..} = namedRecord
    [ "Issue Key" .= toField (gidToText issueKey)
    , "Summary" .= summary
    , "Description" .= toField (formatForPlanningPoker description)
    , "Acceptance Criteria"
      .= toField (formatForPlanningPoker acceptanceCriteria)
    , "Story Points" .= toField storyPoints
    ]

formatForPlanningPoker :: Text -> Text
formatForPlanningPoker = Data.Text.replace "\n" "<br/>"

planningPokerTaskUrl :: PlanningPokerTask -> T.Text
planningPokerTaskUrl PlanningPokerTask {..} =
  "https://app.asana.com/0/0/" <> gidToText issueKey <> "/f"

instance DefaultOrdered PlanningPokerTask where
  headerOrder _ = header
    [ "Issue Key"
    , "Summary"
    , "Description"
    , "Acceptance Criteria"
    , "Story Points"
    ]

toPlanningPokerTask :: Task -> PlanningPokerTask
toPlanningPokerTask task@Task {..} = PlanningPokerTask
  { issueKey = tGid
  , summary = tName
  , description = T.unlines
    [ "<a href='" <> taskUrl task <> "'>" <> tName <> "</a>"
    , "<b>Priority:</b> " <> tshow (fromMaybe 0 (extractPriority task))
    , "<b>Projects</b>"
    , describeMemberships tMemberships
    , "<b>Description</b>"
    , tNotes
    ]
  , acceptanceCriteria = ""
  , storyPoints = extractCost task
  }

describeMemberships :: [Membership] -> Text
describeMemberships = ul . mconcat . map describeMembership
 where
  ul text = "<ul>" <> text <> "</ul>"
  li text = "<li>" <> text <> "</li>"
  describeMembership Membership {..} =
    li $ nName mProject <> maybe "" ((": " <>) . nName) mSection

extractCost :: Task -> Maybe Integer
extractCost t = extractNumberField "cost" t >>= \case
  CustomNumber _ _ mCost -> round <$> mCost
  _ -> Nothing

extractPriority :: Task -> Maybe Integer
extractPriority t = extractNumberField "Priority" t >>= \case
  CustomNumber _ _ mPriority -> round <$> mPriority
  _ -> Nothing
