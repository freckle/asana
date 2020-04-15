module Main (main) where

import RIO

import Asana.Api
import Asana.Api.Gid
import Asana.App
import Data.Csv
import qualified RIO.ByteString.Lazy as RBSL
import qualified RIO.HashMap as HashMap
import qualified RIO.Text as T
import Safe (headMay)

data AppExt = AppExt
  { appProjectId :: Gid
  , appImport :: Maybe FilePath
  }

main :: IO ()
main = do
  app <- loadAppWith $ AppExt <$> parseProjectId <*> parseImport
  runApp app $ do
    AppExt {..} <- asks appExt
    case appImport of
      Nothing -> exportProjectTasks appProjectId
      Just file -> importProjectTasksFromFile file appProjectId

exportProjectTasks :: Gid -> AppM AppExt ()
exportProjectTasks projectId = do
  projectTasks <- getProjectTasks projectId AllTasks
  tasks <- pooledForConcurrentlyN
    maxRequests
    projectTasks
    fetchPlanningPokerTask
  liftIO
    . RBSL.writeFile "planning-poker-export.csv"
    $ encodeDefaultOrderedByName tasks

importProjectTasksFromFile :: FilePath -> Gid -> AppM AppExt ()
importProjectTasksFromFile file projectId = do
  contents <- RBSL.readFile file
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
updatePlanningPokerTaskCost projectTaskMap planningPokerTask@PlanningPokerTask {..}
  = case storyPoints of
    Nothing -> logInfo $ planningPokerTaskLog
      planningPokerTask
      "Does not have a cost. Skipping import."
    Just cost -> case HashMap.lookup issueKey projectTaskMap of
      Nothing -> logWarn $ planningPokerTaskLog
        planningPokerTask
        "Not found in project. Skipping import."

      Just task@Task {..} -> case extractCostField task of
        Just (CustomNumber costFieldGid _ _) -> do
          putCustomField tGid $ CustomNumber costFieldGid "cost" (Just cost)

          logInfo
            $ planningPokerTaskLog planningPokerTask
            $ "cost was updated to "
            <> T.pack (show cost)

        _ -> logWarn $ planningPokerTaskLog
          planningPokerTask
          "No 'cost' field.  Skipping import."

planningPokerTaskLog :: PlanningPokerTask -> T.Text -> Utf8Builder
planningPokerTaskLog task message =
  fromText
    $ "Task \""
    <> planningPokerSummaryFromTaskLink task
    <> "\" <"
    <> planningPokerTaskUrl task
    <> ">: "
    <> message

fromText :: Text -> Utf8Builder
fromText = fromString . T.unpack

data PlanningPokerTask = PlanningPokerTask
  { issueKey :: Gid
  , summary :: Text
  , description :: Text
  , acceptanceCriteria :: Text
  , storyPoints :: Maybe Integer
  }
  deriving stock (Show)

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
      <*> m
      .: "Story Points"

instance ToNamedRecord PlanningPokerTask where
  toNamedRecord task@PlanningPokerTask {..} = namedRecord
    [ "Issue Key" .= toField (gidToText issueKey)
    , "Summary" .= toField (planningPokerTaskLink task)
    , "Description" .= toField description
    , "Acceptance Criteria" .= toField acceptanceCriteria
    , "Story Points" .= toField storyPoints
    ]

planningPokerTaskLink :: PlanningPokerTask -> T.Text
planningPokerTaskLink task@PlanningPokerTask {..} =
  "<a href='" <> planningPokerTaskUrl task <> "'> " <> summary <> "</a>"

-- | Undo formatting of planning poker task link to extract summary
planningPokerSummaryFromTaskLink :: PlanningPokerTask -> T.Text
planningPokerSummaryFromTaskLink task@PlanningPokerTask {..} =
  dropLast 4 $ T.drop
    (T.length $ "<a href='" <> planningPokerTaskUrl task <> "'> ")
    summary
  where dropLast n t = T.take (T.length t - n) t

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

fetchPlanningPokerTask :: Named -> AppM ext PlanningPokerTask
fetchPlanningPokerTask Named {..} = do
  task@Task {..} <- getTask nGid
  pure $ PlanningPokerTask tGid tName tNotes "" $ extractCost task

extractCost :: Task -> Maybe Integer
extractCost t = extractCostField t >>= \case
  CustomNumber _ _ mCost -> mCost
  _ -> Nothing

extractCostField :: Task -> Maybe CustomField
extractCostField Task {..} = headMay $ flip mapMaybe tCustomFields $ \case
  customField@(CustomNumber _ "cost" _) -> Just customField
  _ -> Nothing
