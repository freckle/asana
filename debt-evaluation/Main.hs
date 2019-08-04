-- | A tool for evaluating debt costs
--
-- Add debt evaluation tool

-- This categorizes debt by quick, fill-in, major and thankless. These
-- categories are built in relation to the total set of debt and defined as
-- quadrants in a scatter plot.
--
--    +-----------------------+ high
--   i|           |           |
--   m|   quick   |   major   |
--   p|           |           |
--   a|-----------|-----------|
--   c|           |           |
--   t|  fill-in  | thankless |
--    |           |           |
--    +-----------------------+
-- low         effort
--
-- More info here: https://www.mindtools.com/pages/article/newHTE_95.htm
--
module Main (main) where

import RIO

import Asana.Api
import Asana.App
import Asana.Story
import Control.Monad (guard, when)
import Data.Foldable (maximum, minimum)
import Data.Maybe (mapMaybe)
import RIO.Text (Text)
import Text.Printf (printf)

data Point = Point
  { pId :: Int
  , pUrl :: Text
  , pName :: Text
  , pImpact :: Double
  , pEffort :: Double
  }
  deriving Show

data Priority
  = ThankLess
  -- ^ Try to avoid these activities. Not only do they give little return, they
  -- also soak up time that you should be using on quick wins.
  | FillIn
  -- ^ Don't worry too much about doing these activities – if you have spare
  -- time, do them, but drop them or delegate them if something better comes
  -- along.
  | Quick
  -- ^ Quick wins are the most attractive projects, because they give you a
  -- good return for relatively little effort. Focus on these as much as you
  -- can.
  | Major
  -- ^ Major projects give good returns, but they are time-consuming. This means
  -- that one major project can "crowd out" many quick wins.
  deriving (Show, Eq, Ord)

main :: IO ()
main = do
  app <- loadApp
  runApp app $ do
    projectId <- asks appProjectId

    logDebug "Fetch stories"
    tasks <- getProjectTasks projectId IncompletedTasks
    stories <- pooledForConcurrentlyN maxRequests tasks $ \Named {..} -> do
      story <- fromTask <$> getTask nId
      let url = "<" <> storyUrl projectId story <> ">"
      logInfo . display $ url <> " " <> sName story
      pure story

    logDebug "Calculate points"
    let
      points = flip mapMaybe stories $ \story@Story {..} -> do
        guard $ not sCompleted
        -- Virality compounds impact. Virality itself is predictive of cost
        -- increasing. This makes addressing viral tasks sooner more impactful.
        -- With logarithmic compression, tasks with low virality actually
        -- decrease impact. This biases the system towards viral tasks.
        let virality = log . fromInteger . (+ 1) <$> sVirality
        impact <- (*) <$> (fromInteger <$> sImpact) <*> virality
        cost <- fromInteger <$> sCost
        let url = storyUrl projectId story
        pure $ Point sId url sName impact cost
      (xMid, yMid) = midPoints points
      toActionability Point {..}
        | pImpact > yMid && pEffort < xMid = Quick
        | pImpact > yMid && pEffort >= xMid = Major
        | pImpact <= yMid && pEffort < xMid = FillIn
        | otherwise = ThankLess

    when (null points) $ logWarn "No stories with points"

    logDebug "Label actionability"
    pooledForConcurrentlyN_ maxRequests points $ \point@Point {..} -> do
      let actionability = toActionability point
      putEnumField (fromIntegral pId) $ toActionabilityFieldIds actionability
      logInfo . fromString $ printf
        "Updated actionability %s %s: %s (%.2fc/%.2fi)"
        pUrl
        pName
        (show actionability)
        pEffort
        pImpact

midPoints :: [Point] -> (Double, Double)
midPoints points = (xMid, yMid)
 where
  xMin = minimum $ pEffort <$> points
  xMax = maximum $ pEffort <$> points
  yMin = minimum $ pImpact <$> points
  yMax = maximum $ pImpact <$> points
  xMid = xMax - ((xMax - xMin) / 2)
  yMid = yMax - ((yMax - yMin) / 2)

toActionabilityFieldIds :: Priority -> (Integer, Maybe Integer)
toActionabilityFieldIds = \case
  ThankLess -> (actionabilityFieldId, Just thanklessEnumId)
  FillIn -> (actionabilityFieldId, Just fillinEnumId)
  Quick -> (actionabilityFieldId, Just quickEnumId)
  Major -> (actionabilityFieldId, Just majorEnumId)

actionabilityFieldId :: Integer
actionabilityFieldId = 1109768955052831

thanklessEnumId :: Integer
thanklessEnumId = 1109768955052832

fillinEnumId :: Integer
fillinEnumId = 1109768955052833

quickEnumId :: Integer
quickEnumId = 1109768955052834

majorEnumId :: Integer
majorEnumId = 1109768955052835
