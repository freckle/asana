{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
-- TODO upstream orphan
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | A tool for finding tasks that are taking longer than average
module Main (main) where

import Asana.Prelude

import Asana.Api.CustomField
  (CustomField(CustomNumber), CustomFields(getCustomFields))
import Asana.Api.Gid (AsanaReference(..), Gid, gidToText, textToGid)
import Asana.Api.Named (Named(..))
import Asana.Api.Project (Project(..))
import Asana.Api.Request
import Asana.Api.Task
import Asana.App (AppM, loadAppWith, runApp)
import Data.Aeson
import Data.Aeson.Casing (aesonPrefix, snakeCase)
import Data.Bifunctor (bimap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Data.Hashable (Hashable(..))
import Data.List (find, nub)
import Data.Scientific (Scientific)
import Data.String (fromString)
import Data.Text (intercalate, splitOn)
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import qualified Data.Vector as V
import Network.HTTP.Simple (JSONException)
import Numeric.Natural
import Statistics.Sample (mean)
import UnliftIO.Async (pooledForConcurrentlyN)
import UnliftIO.Exception (throwString)

-- | How many iterations we should look at for finding historical averages (including current)
numberOfIterationsForAverages :: Num p => p
numberOfIterationsForAverages = 5

main :: IO ()
main = do
  app <- loadAppWith $ pure ()
  runApp app $ do
    logDebug "Fetch projects"

    -- Get team iteration projects
    iterations <- mapMaybe parseEducatorIteration <$> getProjects
    logInfo
      $ "Found iterations: "
      <> tshow (fmap iterationNumber iterations)
      :# []

    when (null iterations) $ throwString "Found no Educator iterations!"

    let maxIteration = maximum iterations

    logInfo
      $ "Latest iteration"
      :# ["iterationNumber" .= iterationNumber maxIteration]

    -- Try to find N or so iterations.
    --
    -- - current may be empty (e.g. just started, created ahead of time)
    -- - may have gaps (e.g. if we did a bug-only iteration or a debt-only iteration)
    --
    let
      likelyLastNIterations
        = [iterationNumber maxIteration - (numberOfIterationsForAverages - 1) .. iterationNumber
          maxIteration]
      lastNOrSo =
        filter ((`elem` likelyLastNIterations) . iterationNumber) iterations

    when (length lastNOrSo < 3) $ do
      logError
        $ "Need at least 3 iterations to make this calculation"
        :# ["found" .= fmap iterationNumber lastNOrSo]
      throwString "Not enough iterations!"

    logInfo
      $ "Iterations to search"
      :# ["iterationNumbers" .= fmap iterationNumber lastNOrSo]

    taskGids <- fetchRelevantTaskGids lastNOrSo

    allIterationTasks <- pooledForConcurrentlyN maxRequests taskGids getTask

    -- Get events that happen against each story
    taskStories <-
      fmap (HashMap.fromList . catMaybes)
      $ pooledForConcurrentlyN maxRequests taskGids
      $ \taskId -> fmap (taskId, ) <$> getTaskStories taskId

    -- Assume ics are the assignees of the iteration tasks
    let ics = HashSet.fromList $ catMaybes $ tAssignee <$> allIterationTasks

    let
      -- For historical data
      doneTasks =
        mapMaybe (parseDoneTask ics iterations taskStories) allIterationTasks

      -- Potential current zombies
      wipTasks =
        mapMaybe (parseWipTask ics iterations taskStories) allIterationTasks

    logInfo
      $ "Relevant tasks"
      :# [ "totalTasksInIterations" .= length allIterationTasks
         , "tasksWithStories" .= HashMap.size taskStories
         , "doneTasksInIterations" .= length doneTasks
         , "wipTasksInIterations" .= length wipTasks
         , "icsInIterations"
           .= intercalate ", " (HashSet.toList $ HashSet.map nName ics)
         ]

    for_ doneTasks $ \task -> do
      logDebug $ "Done tasks" :# []
      logDebug $ tshow task :# []

    for_ wipTasks $ \task -> do
      logInfo
        $ "WIP Task"
        :# [ "name" .= wipName task
           , "id" .= wipGid task
           , "roughStartedAt" .= sCreatedAt (wipConsideredStartedBecause task)
           ]

    let
      averageDaysToCompletePoints =
        HashMap.map (toDays . mean . V.fromList . fmap realToFrac)
          $ HashMap.fromListWith (<>)
          $ (doneCost &&& pure . timeToComplete)
          <$> doneTasks

    logInfo $ "Average days to complete points" :# fmap
      (uncurry (.=)
      . bimap ((<> " point(s)") . fromString . show) ((<> " day(s)") . show)
      )
      (HashMap.toList averageDaysToCompletePoints)

    now <- liftIO getCurrentTime

    let
      zombieTasks =
        mapMaybe (parseZombieSummary now averageDaysToCompletePoints) wipTasks

    for_ zombieTasks
      $ \ZombieSummary { daysWip, averageDaysForPoints, zombieTask } -> do
          logInfo
            $ "Zombie Task"
            :# [ "name" .= wipName zombieTask
               , "id" .= wipGid zombieTask
               , "daysWip" .= daysWip
               , "averageDaysForPoints" .= averageDaysForPoints
               ]
          let AsanaStory {..} = wipConsideredStartedBecause zombieTask
          logDebug
            $ "Asana Story (event) which we believe means this story was started at the given time"
            :# [ "gid" .= sGid
               , "createdAt" .= sCreatedAt
               , "createdBy" .= fmap arGid sCreatedBy
               , "text" .= sText
               , "resourceSubtype" .= show sResourceSubtype
               , "assignee" .= fmap arGid sAssignee
               , "project" .= fmap arGid sProject
               ]

-- TODO upstream orphan
instance Hashable Named where
  hashWithSalt s Named {..} = hashWithSalt s nGid
  hash Named {..} = hash nGid

-- | Figure out (rough) start time for the task from its "stories"
--
-- This is the tricky bit... Let's consider a story started, the first time it's
--   * Moved into an actual iteration, or
--   * Assigned to an IC
--
parseRoughStartStory
  :: (Functor t1, Foldable t1, Foldable t2)
  => HashSet.HashSet Named
  -> t1 EducatorIteration
  -> t2 AsanaStory
  -> Maybe AsanaStory
parseRoughStartStory ics iterations taskStories = find
  parseTypicalIndicatorOfTaskStart
  taskStories
 where
  parseTypicalIndicatorOfTaskStart AsanaStory {..} = case sResourceSubtype of
    AddedToProject ->
      any ((`elem` fmap iterationProjectGid iterations) . arGid) sProject
    Assigned -> any ((`HashSet.member` HashSet.map nGid ics) . arGid) sAssignee
    Don'tCare -> False

-- | "Completed" task
data DoneTask = DoneTask
  { doneAt :: UTCTime
  , doneConsideredStartedBecause :: AsanaStory
  , doneCost :: Scientific
  }
  deriving stock Show

timeToComplete :: DoneTask -> NominalDiffTime
timeToComplete DoneTask { doneAt, doneConsideredStartedBecause = AsanaStory { sCreatedAt } }
  = diffUTCTime doneAt sCreatedAt

parseDoneTask
  :: (Functor t1, Foldable t1, Foldable t2)
  => HashSet.HashSet Named
  -> t1 EducatorIteration
  -> HashMap.HashMap Gid (t2 AsanaStory)
  -> Task
  -> Maybe DoneTask
parseDoneTask ics iterations taskStoriesLookup Task { tCompleted, tCompletedAt, tCustomFields, tGid }
  = do
    guard tCompleted
    doneAt <- tCompletedAt
    doneCost <- parseCostFromCustomFields tCustomFields
    taskStories <- HashMap.lookup tGid taskStoriesLookup
    doneConsideredStartedBecause <- parseRoughStartStory
      ics
      iterations
      taskStories
    pure DoneTask { doneAt, doneCost, doneConsideredStartedBecause }

parseCostFromCustomFields :: CustomFields -> Maybe Scientific
parseCostFromCustomFields = listToMaybe . mapMaybe parseCost . getCustomFields

parseCost :: CustomField -> Maybe Scientific
parseCost = \case
  CustomNumber gid _ mCost | gid == costCustomFieldGid -> mCost
  _ -> Nothing

-- | In progress task
data WipTask = WipTask
  { wipName :: Text
  , wipGid :: Gid
  , wipTotalCost :: Scientific
  -- ^
  --
  -- We just care about total cost not carry-over.
  --
  , wipConsideredStartedBecause :: AsanaStory
  }

parseWipTask
  :: (Functor t1, Foldable t1, Foldable t2)
  => HashSet.HashSet Named
  -> t1 EducatorIteration
  -> HashMap.HashMap Gid (t2 AsanaStory)
  -> Task
  -> Maybe WipTask
parseWipTask ics iterations taskStoriesLookup Task { tName, tCompleted, tCustomFields, tGid }
  = do
    guard $ not tCompleted
    wipTotalCost <- parseCostFromCustomFields tCustomFields
    taskStories <- HashMap.lookup tGid taskStoriesLookup
    wipConsideredStartedBecause <- parseRoughStartStory
      ics
      iterations
      taskStories
    pure WipTask { wipTotalCost, wipConsideredStartedBecause, wipName, wipGid }

 where
  wipName = tName
  wipGid = tGid

data ZombieSummary = ZombieSummary
  { zombieTask :: WipTask
  , daysWip :: Int
  , averageDaysForPoints :: Int
  }

parseZombieSummary
  :: UTCTime -> HashMap.HashMap Scientific Int -> WipTask -> Maybe ZombieSummary
parseZombieSummary now averageTimeForCost wipTask@WipTask { wipTotalCost, wipConsideredStartedBecause = AsanaStory { sCreatedAt } }
  = do
    averageDaysForPoints <- HashMap.lookup wipTotalCost averageTimeForCost
    guard $ daysWip > averageDaysForPoints
    pure ZombieSummary { zombieTask = wipTask, daysWip, averageDaysForPoints }
  where daysWip = toDays $ realToFrac $ diffUTCTime now sCreatedAt

costCustomFieldGid :: Gid
costCustomFieldGid = textToGid "293622495894291"

toDays :: Double -> Int
toDays = floor . (/ 86400)

data EducatorIteration = EducatorIteration
  { iterationNumber :: Natural
  , iterationProjectCreatedAt :: UTCTime
  , iterationProjectGid :: Gid
  }
  deriving stock (Eq, Show)

instance Ord EducatorIteration where
  compare a b = compare (iterationNumber a) $ iterationNumber b

parseEducatorIteration :: Project -> Maybe EducatorIteration
parseEducatorIteration Project { pName, pCreatedAt = iterationProjectCreatedAt, pGid = iterationProjectGid }
  = case splitOn " " pName of
    ["Educator", rawIterationNumber] ->
      fmap mkIteration $ readMaybe $ unpack rawIterationNumber
    _ -> Nothing
 where
  mkIteration iterationNumber = EducatorIteration
    { iterationNumber
    , iterationProjectCreatedAt
    , iterationProjectGid
    }

fetchRelevantTaskGids :: Traversable t => t EducatorIteration -> AppM () [Gid]
fetchRelevantTaskGids iterations =
  fmap (nub . concat)
    . pooledForConcurrentlyN maxRequests iterations
    $ \EducatorIteration { iterationProjectGid } -> do
        logDebug . fromString $ "Project tasks: " <> show
          (gidToText iterationProjectGid)
        fmap nGid <$> getProjectTasks iterationProjectGid AllTasks

getProjects
  :: (MonadUnliftIO m, MonadLogger m, MonadReader env m, HasAsanaAccessKey env)
  => m [Project]
getProjects = getProjectsForTeam educatorTeam
  where educatorTeam = "1201325186562310"

getProjectsForTeam
  :: (MonadUnliftIO m, MonadLogger m, MonadReader env m, HasAsanaAccessKey env)
  => String
  -> m [Project]
getProjectsForTeam team = getAllParams
  (unpack "/projects/")
  [("team", team), ("opt_fields", "created_at,name")]

-- TODO upstream stories stuff
-- "Story" is Asana's term for an event that happens against some entity.
-- For this logic we only care about a task's stories.

-- opt_fields=project,created_at,created_by,text,type,resource_subtype,assignee


-- |
--
-- Only enumerates the types we care about for this logic.
--
data StoryResourceSubtype
  = AddedToProject
  | Assigned
  | Don'tCare
  deriving stock (Generic, Show, Eq)

instance FromJSON StoryResourceSubtype where
  parseJSON = \case
    String "added_to_project" -> pure AddedToProject
    String "assigned" -> pure AddedToProject
    String _ -> pure Don'tCare
    _ -> mzero

-- |
--
-- Named 'AsanaStory' to disambiguate with this codebase's 'Story' which
-- represents an Asana 'Task' (i.e. a 'Task' with Freckle-specific fields).
--
data AsanaStory = AsanaStory
  { sGid :: Gid
  , sCreatedAt :: UTCTime
  , sCreatedBy :: Maybe AsanaReference
  , sText :: Maybe Text
  , sResourceSubtype :: StoryResourceSubtype
  , sAssignee :: Maybe AsanaReference
  -- ^
  --
  -- Available when @resource_subtype@ is @assigned@
  --
  , sProject :: Maybe AsanaReference
  -- ^
  --
  -- Available when @resource_subtype@ is @added_to_project@
  --
  }
  deriving stock (Eq, Generic, Show)

instance FromJSON AsanaStory where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

getTaskStories
  :: (MonadUnliftIO m, MonadLogger m, MonadReader env m, HasAsanaAccessKey env)
  => Gid
  -> m (Maybe [AsanaStory])
getTaskStories taskId = fmap Just fetch `catch` ignoreAndLogError
 where
  fetch = getAllParams
    (unpack $ "/tasks/" <> gidToText taskId <> "/stories")
    [ ( "opt_fields"
      , "project,created_at,created_by,text,resource_subtype,assignee,text"
      )
    ]

  ignoreAndLogError :: (MonadLogger m) => JSONException -> m (Maybe a)
  ignoreAndLogError err = do
    logWarn $ tshow err :# ["fetchingTask" .= taskId]
    pure Nothing
