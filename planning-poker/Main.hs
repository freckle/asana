module Main (main) where

import RIO

import Asana.Api
import Asana.Api.Gid
import Asana.App
import Data.Csv
import qualified Data.Text
import qualified RIO.ByteString.Lazy as RBSL
import qualified RIO.HashMap as HashMap
import RIO.List (sortOn)
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

  tasks <- pooledForConcurrentlyN maxRequests projectTasks (getTask . nGid)

  let planningPokerTasks = map toPlanningPokerTask (sortByPriority tasks)

  liftIO
    . RBSL.writeFile "planning-poker-export.csv"
    $ encodeDefaultOrderedByName planningPokerTasks

  for_ planningPokerTasks $ \task -> logInfo $ planningPokerTaskLog
    summary
    task
    "exported to planning-poker-export.csv"

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
      planningPokerSummaryFromTaskLink
      planningPokerTask
      "Does not have a cost. Skipping import."
    Just cost -> case HashMap.lookup issueKey projectTaskMap of
      Nothing -> logWarn $ planningPokerTaskLog
        planningPokerSummaryFromTaskLink
        planningPokerTask
        "Not found in project. Skipping import."

      Just task@Task {..} -> case extractCostField task of
        Just (CustomNumber costFieldGid _ _) -> do
          putCustomField tGid
            $ CustomNumber costFieldGid "cost" (Just $ fromIntegral cost)

          logInfo
            $ planningPokerTaskLog
                planningPokerSummaryFromTaskLink
                planningPokerTask
            $ "cost was updated to "
            <> T.pack (show cost)

        _ -> logWarn $ planningPokerTaskLog
          planningPokerSummaryFromTaskLink
          planningPokerTask
          "No 'cost' field.  Skipping import."

planningPokerTaskLog
  :: (PlanningPokerTask -> Text) -> PlanningPokerTask -> T.Text -> Utf8Builder
planningPokerTaskLog format task message =
  fromText
    $ "Task \""
    <> format task
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
    , "Description" .= toField (formatForPlanningPoker description)
    , "Acceptance Criteria"
      .= toField (formatForPlanningPoker acceptanceCriteria)
    , "Story Points" .= toField storyPoints
    ]

formatForPlanningPoker :: Text -> Text
formatForPlanningPoker = Data.Text.replace "\n" "<br/>"

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

toPlanningPokerTask :: Task -> PlanningPokerTask
toPlanningPokerTask task@Task {..} = PlanningPokerTask
  { issueKey = tGid
  , summary = tName
  , description = T.unlines
    [ "<b>Priority:</b> " <> tshow (fromMaybe 0 (extractPriority task))
    , "<b>Projects</b>"
    , describeMemberships tMemberships
    , "<b>Description</b>"
    , tNotes
    ]
  , acceptanceCriteria = ""
  , storyPoints = extractCost task
  }

describeMemberships = ul . mconcat . map describeMembership
 where
  ul text = "<ul>" <> text <> "</ul>"
  li text = "<li>" <> text <> "</li>"
  describeMembership Membership {..} =
    li $ nName mProject <> maybe "" ((": " <>) . nName) mSection

extractCost :: Task -> Maybe Integer
extractCost t = extractCostField t >>= \case
  CustomNumber _ _ mCost -> round <$> mCost
  _ -> Nothing

extractCostField :: Task -> Maybe CustomField
extractCostField Task {..} = headMay $ flip mapMaybe tCustomFields $ \case
  customField@(CustomNumber _ "cost" _) -> Just customField
  _ -> Nothing

sortByPriority :: [Task] -> [Task]
sortByPriority = sortOn (Down . extractPriority)

extractPriority :: Task -> Maybe Integer
extractPriority t = extractPriorityField t >>= \case
  CustomNumber _ _ mPriority -> round <$> mPriority
  _ -> Nothing

extractPriorityField :: Task -> Maybe CustomField
extractPriorityField Task {..} = headMay $ flip mapMaybe tCustomFields $ \case
  customField@(CustomNumber _ "Priority" _) -> Just customField
  _ -> Nothing
