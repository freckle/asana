-- | A tool for measuring cycle time of tasks
--
-- Cycle time is the difference between creation date and completion date.
--
module Main (main) where

import Asana.Prelude

import Asana.Api.Gid (Gid, gidToText)
import Asana.Api.Named
import Asana.Api.Project (Project(..))
import Asana.Api.Request
import Asana.Api.Task
import Asana.App
  (AppM, appExt, loadAppWith, parseBugProjectId, parseDate, runApp)
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (toStrict)
import qualified Data.Csv as Csv
import Data.List (intercalate, nub)
import qualified Data.Map.Strict as Map
import Data.String (fromString)
import Data.Text (isPrefixOf)
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Statistics.Quantile (median, medianUnbiased)
import Statistics.Sample (kurtosis, mean, skewness, stdDev)
import Statistics.Sample.Histogram (histogram)
import System.IO.Temp (emptySystemTempFile)
import Text.Printf (printf)
import UnliftIO.Async (pooledForConcurrentlyN)

data AppExt = AppExt
  { appFrom :: UTCTime
  , appTo :: UTCTime
  , appBugProject :: Gid
  }

main :: IO ()
main = do
  app <-
    loadAppWith
    $ AppExt
    <$> parseDate "from"
    <*> parseDate "to"
    <*> parseBugProjectId
  runApp app $ do
    logDebug "Fetch projects"
    dateFrom <- asks $ appFrom . appExt
    dateTo <- asks $ appTo . appExt

    projects <- filter (approvedProject dateFrom dateTo) <$> getProjects
    logInfo $ fromString $ show $ pName <$> projects

    taskGids <- fetchRelevantTaskGids projects

    tasks <- pooledForConcurrentlyN maxRequests taskGids getTask
    logDebug "Write task CSV"
    writeTaskCsv tasks

    let
      cycleTimes = zscoreFilter . V.fromList . fmap realToFrac $ mapMaybe
        mayCycleTime
        tasks

    when (null cycleTimes) $ logWarn "No lead time to compute"

    logDebug "Write histogram CSV"
    writeHistogramCsv cycleTimes

    logDebug "Display Lead Time"
    let
      fmt = intercalate
        "\n"
        [ "Cycle Time"
        , "- mean:     %d days"
        , "- mediam:   %d days"
        , "- skewness: %.3f"
        , "- kurtosis: %.3f"
        , "- stdDev:   %d days"
        , "- max:      %d days"
        , "- min:      %d days"
        ]
    logInfo . fromString $ printf
      fmt
      (toDays $ mean cycleTimes)
      (toDays $ median medianUnbiased cycleTimes)
      (skewness cycleTimes)
      (kurtosis cycleTimes)
      (toDays $ stdDev cycleTimes)
      (toDays $ maximum cycleTimes)
      (toDays $ minimum cycleTimes)

zscoreFilter :: Vector Double -> Vector Double
zscoreFilter p =
  let
    av = mean p
    sdev = stdDev p
    zs x = (x - av) / sdev
  in V.filter ((<= 3) . zs) p

toDays :: Double -> Int
toDays = floor . (/ 86400)

approvedProject :: UTCTime -> UTCTime -> Project -> Bool
approvedProject dateFrom dateTo project =
  dateFrom <= createdAt && createdAt < dateTo && isValidIteration
    (pName project)
  where createdAt = pCreatedAt project

isValidIteration :: Text -> Bool
isValidIteration name =
  isPrefixOf "Iteration" name
    || isPrefixOf "Student" name
    || isPrefixOf "Educator" name
    || isPrefixOf "Platform" name

mayCycleTime :: Task -> Maybe NominalDiffTime
mayCycleTime task = do
  completedAt <- tCompletedAt task
  -- FIXME: Sometimes this is true
  guard $ completedAt > tCreatedAt task
  Just . diffUTCTime completedAt $ tCreatedAt task

writeHistogramCsv :: Vector Double -> AppM ext ()
writeHistogramCsv cycleTimes = do
  filePath <- liftIO $ emptySystemTempFile ".csv"
  let histogramCsv = uncurry V.zip $ histogram @_ @_ @Double 100 cycleTimes
  liftIO $ BS.writeFile filePath $ toStrict $ Csv.encode $ V.toList histogramCsv
  logInfo $ "Histogram written" :# ["path" .= filePath]

writeTaskCsv :: [Task] -> AppM ext ()
writeTaskCsv tasks = do
  filePath <- liftIO $ emptySystemTempFile ".csv"
  let
    toRecord task = Map.fromList
      [ ("name" :: String, unpack $ tName task)
      , ("task ID" :: String, unpack $ taskUrl task)
      , ( "cycle time"
        , maybe "" (show . realToFrac @_ @Double) $ mayCycleTime task
        )
      ]
  liftIO
    $ BS.writeFile filePath
    $ toStrict
    $ Csv.encodeByName (V.fromList ["name", "task ID", "cycle time"])
    $ toRecord
    <$> tasks
  logInfo $ "Tasks written" :# ["path" .= filePath]

fetchRelevantTaskGids :: Traversable t => t Project -> AppM AppExt [Gid]
fetchRelevantTaskGids projects = do
  taskGids <-
    fmap (nub . concat)
    . pooledForConcurrentlyN maxRequests projects
    $ \project -> do
        logDebug . fromString $ "Project tasks: " <> show
          (gidToText $ pGid project)
        fmap nGid <$> getProjectTasks (pGid project) AllTasks
  bugProjectGid <- asks $ appBugProject . appExt
  bugTaskGids <- fmap nGid <$> getProjectTasks bugProjectGid AllTasks

  pure $ filter (`notElem` bugTaskGids) taskGids

getProjects
  :: (MonadUnliftIO m, MonadLogger m, MonadReader env m, HasAsanaAccessKey env)
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
  :: (MonadUnliftIO m, MonadLogger m, MonadReader env m, HasAsanaAccessKey env)
  => String
  -> m [Project]
getProjectsForTeam team = getAllParams
  (unpack "/projects/")
  [("team", team), ("opt_fields", "created_at,name")]
