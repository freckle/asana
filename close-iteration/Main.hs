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
      isCarried = isJust . sCarryOver
      (completedAndCarriedStories, incompleteStories) =
        partition sCompleted stories

      (carriedStories, completedStories) =
        partition isCarried completedAndCarriedStories
      completedCost = sum $ mapMaybe sCost completedStories
      completedCarryOver = sum $ mapMaybe sCarryOver carriedStories

      incompleteCost = sum $ mapMaybe sCost incompleteStories
      incompleteCarryOver = sum $ mapMaybe sCarryOver incompleteStories

      completedWithCarry = completedCost + completedCarryOver

      totalCompletedCost =
        sum [completedWithCarry, incompleteCost - incompleteCarryOver]

      totalCommittedCost = sum [completedWithCarry, incompleteCost]

      storiesCompletedWithCarry =
        length completedStories + length carriedStories

    hPutBuilder stdout . getUtf8Builder $ foldMap
      (<> "\n")
      [ ""
      , "Completed points: "
      <> display completedWithCarry
      <> " ("
      <> display completedCarryOver
      <> " were carry-over)"
      , "Completed tasks: "
      <> display storiesCompletedWithCarry
      <> " ("
      <> display (length carriedStories)
      <> " were carry-over)"
      , ""
      , "Incomplete points: " <> display incompleteCarryOver
      , "Incomplete tasks: "
      <> display (length incompleteStories)
      <> " ("
      <> display incompleteCost
      <> " total cost)"
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
