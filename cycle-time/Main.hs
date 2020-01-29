-- | A tool for measuring cycle time of tasks
--
-- Cycle time is the difference between creation date and completion date.
--
module Main (main) where

import RIO

import Asana.Api
import Asana.Api.Gid (gidToText, textToGid)
import Asana.Api.Project (Project(pCreatedAt, pGid, pName), getProjects)
import Asana.App (loadApp, runApp)
import Control.Monad (when)
import Data.Csv as Csv
import Data.Foldable (maximum, minimum)
import Data.List (intercalate, nub)
import qualified Data.Vector as V
import RIO.ByteString (writeFile)
import RIO.ByteString.Lazy (toStrict)
import RIO.Text (isPrefixOf)
import RIO.Time (NominalDiffTime, diffUTCTime, toGregorian, utctDay)
import Statistics.Quantile (median, medianUnbiased)
import Statistics.Sample (kurtosis, mean, skewness, stdDev)
import Statistics.Sample.Histogram (histogram)
import System.IO.Temp (emptySystemTempFile)
import Text.Printf (printf)

main :: IO ()
main = do
  app <- loadApp
  runApp app $ do
    logDebug "Fetch projects"
    projects <- filter approvedProject <$> getProjects

    taskGids <-
      fmap (nub . concat)
      . pooledForConcurrentlyN maxRequests projects
      $ \project -> do
          logDebug . fromString $ "Project tasks: " <> show
            (gidToText $ pGid project)
          fmap nGid <$> getProjectTasks (pGid project) AllTasks
    bugTaskGids <-
      fmap nGid <$> getProjectTasks (textToGid "56224185180675") AllTasks

    let nonBugGids = filter (`notElem` bugTaskGids) taskGids

    cycleTimes <-
      zscoreFilter
      . V.fromList
      . fmap realToFrac
      . mapMaybe mayCycleTime
      <$> pooledForConcurrentlyN maxRequests nonBugGids getTask

    when (null cycleTimes) $ logWarn "No cycle time to compute"

    logDebug "Write histogram CSV"
    filePath <- liftIO $ emptySystemTempFile ".csv"
    let histogramCsv = uncurry V.zip $ histogram @_ @_ @Double 100 cycleTimes
    writeFile filePath . toStrict . Csv.encode $ V.toList histogramCsv
    logInfo . fromString $ "Histogram written to " <> filePath

    logDebug "Display Cycle Time"
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

approvedProject :: Project -> Bool
approvedProject project = year == 2018 && isPrefixOf
  "Iteration"
  (pName project)
  where (year, _, _) = toGregorian $ utctDay (pCreatedAt project)

mayCycleTime :: Task -> Maybe NominalDiffTime
mayCycleTime task = do
  completedAt <- tCompletedAt task
  -- FIXME: Sometimes this is true
  guard $ completedAt > tCreatedAt task
  Just . diffUTCTime completedAt $ tCreatedAt task
