module Main (main) where

import Prelude

import Asana.Api
import Asana.App
import Asana.Story
import Control.Monad (when)
import Control.Monad.Logger
import Data.List (partition)
import Data.Maybe (isJust, isNothing, mapMaybe)
import Data.Semigroup ((<>))
import FrontRow.App
import UnliftIO.Async (pooledForConcurrentlyN)

main :: IO ()
main = do
  app <- loadApp
  runApp app $ do
    projectId <- asks appProjectId
    perspective <- asks appPerspective
    tasks <- getProjectTasks projectId AllTasks

    stories <- pooledForConcurrentlyN maxRequests tasks $ \Named {..} -> do
      story@Story {..} <- fromTask <$> getTask nId
      let url = "<" <> storyUrl projectId story <> ">"
      logInfoN $ url <> " " <> sName

      let incompleteNoCarry = not sCompleted && isNothing sCarryOver
      when incompleteNoCarry
        $ logWarnN
        $ "No carry over on incomplete story: "
        <> url

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

    liftIO $ putStrLn $ unlines
      [ "Completed"
      , "- new points: " <> show completedCost
      , "- carried over points: " <> show completedCarryOver
      , "- new stories: " <> show (length completedStories)
      , "- carried over stories: " <> show (length carriedStories)
      , "Incomplete"
      , "- points completed: " <> show (incompleteCost - incompleteCarryOver)
      , "- carry over points: " <> show incompleteCarryOver
      , "- carry over stories: " <> show (length incompleteStories)
      , ""
      , show
        (completedCost
        + completedCarryOver
        + (incompleteCost - incompleteCarryOver)
        )
      <> " / "
      <> show (completedCost + completedCarryOver + incompleteCost)
      ]
