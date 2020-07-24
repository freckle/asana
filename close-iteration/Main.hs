{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import RIO

import Asana.Api
import Asana.Api.Gid (Gid)
import Asana.App
import Asana.Story
import Control.Monad (when)
import Data.List (partition)
import Data.Maybe (isJust, isNothing, mapMaybe)
import Data.Semigroup ((<>))

data AppExt = AppExt
  { appProjectId :: Gid
  , appPerspective :: Perspective
  }

main :: IO ()
main = do
  app <- loadAppWith $ AppExt <$> parseProjectId <*> parsePessimistic
  runApp app $ do
    projectId <- asks $ appProjectId . appExt
    perspective <- asks $ appPerspective . appExt
    tasks <- getProjectTasks projectId AllTasks

    let
      processStories =
        fmap catMaybes . pooledForConcurrentlyN maxRequests tasks
    stories <- processStories $ \Named {..} -> do
      mStory <- fromTask <$> getTask nGid
      for mStory $ \story@Story {..} -> do
        let url = "<" <> storyUrl projectId story <> ">"
        logInfo . display $ url <> " " <> sName

        let
          incompleteNoCarry =
            not sCompleted && isNothing sCarryOver && maybe True (> 0) sCost
        when incompleteNoCarry
          $ logWarn
          $ "No carry over on incomplete story: "
          <> display url

        pure $ case (incompleteNoCarry, perspective) of
          (True, Pessimistic) -> story { sCarryOver = sCost }
          (_, _) -> story

    let capitalizedStats = statStories $ filter sCapitalized stories
    hPutBuilder stdout $ getUtf8Builder $ foldMap
      ("\n" <>)
      [ "Capitalized"
      , "- " <> display (completed capitalizedStats) <> " / " <> display
        (commitment capitalizedStats)
      ]
    printStats $ statStories stories

printStats :: MonadIO m => CompletionStats -> m ()
printStats stats@CompletionStats {..} =
  hPutBuilder stdout $ getUtf8Builder $ foldMap
    ("\n" <>)
    [ "Completed"
    , "- new points: " <> display completedNewCost
    , "- carried over points: " <> display completedCarryOver
    , "- new stories: " <> display completedNewCount
    , "- carried over stories: " <> display carriedCount
    , "Incomplete"
    , "- points completed: " <> display (incompleteCost - incompleteCarryOver)
    , "- carry over points: " <> display incompleteCarryOver
    , "- carry over stories: " <> display incompleteCount
    , ""
    , display (completed stats) <> " / " <> display (commitment stats)
    ]

completed :: CompletionStats -> Integer
completed CompletionStats {..} =
  completedNewCost + completedCarryOver + (incompleteCost - incompleteCarryOver)

commitment :: CompletionStats -> Integer
commitment CompletionStats {..} =
  completedNewCost + completedCarryOver + incompleteCost

statStories :: [Story] -> CompletionStats
statStories stories = CompletionStats
  { completedNewCost
  , completedCarryOver
  , incompleteCost
  , incompleteCarryOver
  , completedNewCount = length completedStories
  , carriedCount = length carriedStories
  , incompleteCount = length incompleteStories
  }
 where
  isCarried = isJust . sCarryOver
  (completedAndCarriedStories, incompleteStories) =
    partition sCompleted stories

  (carriedStories, completedStories) =
    partition isCarried completedAndCarriedStories
  completedNewCost = sum $ mapMaybe sCost completedStories
  completedCarryOver = sum $ mapMaybe sCarryOver carriedStories

  incompleteCost = sum $ mapMaybe sCost incompleteStories
  incompleteCarryOver = sum $ mapMaybe sCarryOver incompleteStories

data CompletionStats = CompletionStats
  { completedNewCount :: Int
  , carriedCount :: Int
  , incompleteCount :: Int
  , completedNewCost :: Integer
  , completedCarryOver :: Integer
  , incompleteCost :: Integer
  , incompleteCarryOver :: Integer
  }
