module Main (main) where

import RIO

import Asana.Api
import Asana.App
import Asana.Story
import Control.Monad (when)
import Data.List (partition)
import Data.Maybe (isJust, isNothing, mapMaybe)
import Data.Semigroup ((<>))

main :: IO ()
main = do
  app <- loadApp
  runApp app $ do
    projectId <- asks appProjectId
    perspective <- asks appPerspective
    tasks <- getProjectTasks projectId AllTasks

    let
      processStories =
        fmap catMaybes . pooledForConcurrentlyN maxRequests tasks
    stories <- processStories $ \Named {..} -> do
      mStory <- fromTask <$> getTask nGid
      for mStory $ \story@Story {..} -> do
        let url = "<" <> storyUrl projectId story <> ">"
        logInfo . display $ url <> " " <> sName

        let incompleteNoCarry = not sCompleted && isNothing sCarryOver
        when incompleteNoCarry
          $ logWarn
          $ "No carry over on incomplete story: "
          <> display url

        pure $ case (incompleteNoCarry, perspective) of
          (True, Pessimistic) -> story { sCarryOver = sCost }
          (_, _) -> story

    let
      (completedStories, incompleteStories) = partition sCompleted stories

      incompleteCost = sum $ mapMaybe sCost incompleteStories
      incompleteCarryOver = sum $ mapMaybe sCarryOver incompleteStories

      (completedStoriesCarried, completedStoriesNew) =
        partition (isJust . sCarryOver) completedStories

      completedCostCarried = sum $ mapMaybe sCarryOver completedStoriesCarried
      completedCostNew = sum $ mapMaybe sCost completedStoriesNew
      completedCostPartial = incompleteCost - incompleteCarryOver

      totalCompletedCost =
        sum [completedCostCarried, completedCostNew, completedCostPartial]
      totalCommittedCost = sum [totalCompletedCost, incompleteCarryOver]

    hPutBuilder stdout . getUtf8Builder $ foldMap
      (<> "\n")
      [ ""
      , "Completed points: "
      <> display totalCompletedCost
      <> " ("
      <> display completedCostCarried
      <> " carry-over"
      <> ", "
      <> display completedCostNew
      <> " new"
      <> ", "
      <> display completedCostPartial
      <> " in carrying tasks"
      <> ")"
      , "Completed tasks: "
      <> display (length completedStories)
      <> " ("
      <> display (length completedStoriesCarried)
      <> " carry-over"
      <> ", "
      <> display (length completedStoriesNew)
      <> " new)"
      , ""
      , "Incomplete points: " <> display incompleteCarryOver
      , "Incomplete tasks: "
      <> display (length incompleteStories)
      <> " (worth "
      <> display incompleteCost
      <> " total)"
      , ""
      , "Velocity: "
      <> display totalCompletedCost
      <> "/"
      <> display totalCommittedCost
      <> " ("
      <> display (velocity totalCompletedCost totalCommittedCost)
      <> "%)"
      ]

velocity :: Integer -> Integer -> Double
velocity completed committed =
  (fromIntegral completed / fromIntegral committed) * 100
