{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Asana.Prelude

import Asana.Api.Gid (Gid)
import Asana.Api.Named
import Asana.Api.Request
import Asana.Api.Task
import Asana.App
import Asana.Story
import Control.Monad.Trans.Maybe (MaybeT(MaybeT), runMaybeT)
import Data.List (partition)
import qualified Data.Text as T
import UnliftIO.Async (pooledForConcurrentlyN)

data AppExt = AppExt
  { appProjectId :: Gid
  , appIgnoreNoCanDo :: Bool
  , appSubprojectName :: Maybe String
  }

main :: IO ()
main = do
  app <-
    loadAppWith
    $ AppExt
    <$> parseProjectId
    <*> parseIgnoreNoCanDo
    <*> parseSubprojectName
  runApp app $ do
    projectId <- asks $ appProjectId . appExt
    projectTasks <- getProjectTasks projectId AllTasks

    tasks <- do
      allTasks <- pooledForConcurrentlyN
        maxRequests
        projectTasks
        (getTask . nGid)

      case appSubprojectName (appExt app) of
        Nothing -> pure allTasks
        Just subprojectName ->
          pure $ flip filter allTasks $ \Task { tMemberships } ->
            flip any tMemberships $ \Membership { mProject } ->
              T.toLower (nName mProject) == T.toLower (T.pack subprojectName)

    stories <- fmap catMaybes . for tasks $ \task -> runMaybeT $ do
      story@Story {..} <- MaybeT $ pure $ fromTask (Just projectId) task
      let url = "<" <> storyUrl projectId story <> ">"
      MaybeT $ do
        logInfo $ "Story" :# ["name" .= sName]
        when sCompleted
          $ logWarn
          $ "Completed story in iteration: "
          :# ["url" .= url]
        unless (maybe True isFib sCost)
          $ logWarn
          $ "Story's cost is not a Fibonacci number"
          :# ["url" .= url]
        when (isNothing sCost)
          $ logWarn
          $ "Story is not costed"
          :# ["url" .= url]
        case sAssignee of
          Nothing -> do
            logWarn $ "Story has no assignee" :# ["url" .= url]
            pure Nothing
          Just _ -> mayCanDo story

    let
      isCarried = isJust . sCarryIn
      (carriedStories, iterationStories) = partition isCarried stories
      iterationCost = sum $ mapMaybe sCost iterationStories
      iterationNum = length iterationStories
      carriedCost = sum $ mapMaybe sCarryIn carriedStories
      carriedNum = length carriedStories

    liftIO $ putStrLn $ foldMap
      ("\n" <>)
      [ "New Story Points"
      , "  " <> show iterationCost <> " (" <> show iterationNum <> " stories)"
      , "Carryover Story Points"
      , "  " <> show carriedCost <> " (" <> show carriedNum <> " stories)"
      , "Total"
      , "  "
      <> show (iterationCost + carriedCost)
      <> " ("
      <> show (iterationNum + carriedNum)
      <> " stories)"
      ]

makeUrl :: Gid -> Story -> Text
makeUrl projectId story = "<" <> storyUrl projectId story <> ">"

mayCanDo :: Story -> AppM AppExt (Maybe Story)
mayCanDo story = do
  projectId <- asks $ appProjectId . appExt
  let url = makeUrl projectId story
  case sCanDo story of
    Nothing -> do
      logWarn $ "Story does not have a 'can do?'" :# ["url" .= url]
      ignoreNoCanDo <- asks $ appIgnoreNoCanDo . appExt
      pure $ if ignoreNoCanDo then Nothing else Just story
    Just canDo -> do
      unless canDo $ logWarn $ "Story marked as can't do" :# ["url" .= url]
      pure $ Just story

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

isFib :: Integer -> Bool
isFib i = elem i $ takeWhile (<= i) fibs
