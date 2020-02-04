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

    hPutBuilder stdout . getUtf8Builder $ foldMap
      ("\n" <>)
      [ "Completed"
      , "- new points: " <> display completedCost
      , "- carried over points: " <> display completedCarryOver
      , "- new stories: " <> display (length completedStories)
      , "- carried over stories: " <> display (length carriedStories)
      , "Incomplete"
      , "- points completed: " <> display (incompleteCost - incompleteCarryOver)
      , "- carry over points: " <> display incompleteCarryOver
      , "- carry over stories: " <> display (length incompleteStories)
      , ""
      , display
        (completedCost
        + completedCarryOver
        + (incompleteCost - incompleteCarryOver)
        )
      <> " / "
      <> display (completedCost + completedCarryOver + incompleteCost)
      ]
