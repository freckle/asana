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

import Asana.Prelude

import Asana.Api.CustomField
import Asana.Api.Gid (Gid, textToGid)
import Asana.Api.Named
import Asana.Api.Request
import Asana.Api.Task
import Asana.App
import Asana.Story
import Data.String (fromString)
import Text.Printf (printf)
import UnliftIO.Async (pooledForConcurrentlyN, pooledForConcurrentlyN_)

newtype AppExt = AppExt
  { appProjectId :: Gid
  }

data Point = Point
  { pGid :: Gid
  , pUrl :: Text
  , pName :: Text
  , pImpact :: Double
  , pEffort :: Double
  }
  deriving stock Show

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
  deriving stock (Show, Eq, Ord)

main :: IO ()
main = do
  app <- loadAppWith $ AppExt <$> parseProjectId
  runApp app $ do
    projectId <- asks $ appProjectId . appExt

    logDebug "Fetch stories"
    tasks <- getProjectTasks projectId IncompletedTasks
    let
      processStories =
        fmap catMaybes . pooledForConcurrentlyN maxRequests tasks
    stories <- processStories $ \Named {..} -> do
      mStory <- fromTask Nothing <$> getTask nGid
      for mStory $ \story -> do
        logInfo
          $ "Story"
          :# ["url" .= storyUrl projectId story, "name" .= sName story]
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
        pure $ Point sGid url sName impact cost
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
      putCustomField pGid $ toActionabilityField actionability
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

toActionabilityField :: Priority -> CustomField
toActionabilityField =
  CustomEnum actionabilityFieldId "Actionability" actionabilityEnumOptions
    . Just
    . tshow

actionabilityFieldId :: Gid
actionabilityFieldId = textToGid "1109768955052831"

actionabilityEnumOptions :: [EnumOption]
actionabilityEnumOptions =
  [ EnumOption (textToGid "1109768955052832") $ tshow ThankLess
  , EnumOption (textToGid "1109768955052833") $ tshow FillIn
  , EnumOption (textToGid "1109768955052834") $ tshow Quick
  , EnumOption (textToGid "1109768955052835") $ tshow Major
  ]
