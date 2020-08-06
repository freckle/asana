module Main (main) where

import RIO

import Asana.Api
import Asana.Api.Gid (Gid)
import Asana.App
import Asana.Story
import Control.Monad (unless, when)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT), runMaybeT)
import Data.List (partition, tail, zipWith)
import Data.Maybe (isJust, isNothing, mapMaybe)
import Data.Semigroup ((<>))

data AppExt = AppExt
  { appProjectId :: Gid
  , appIgnoreNoCanDo :: Bool
  }

main :: IO ()
main = do
  app <- loadAppWith $ AppExt <$> parseProjectId <*> parseIgnoreNoCanDo
  runApp app $ do
    projectId <- asks $ appProjectId . appExt
    tasks <- getProjectTasks projectId AllTasks

    let
      processStories =
        fmap catMaybes . pooledForConcurrentlyN maxRequests tasks

    stories <- processStories $ \Named {..} -> runMaybeT $ do
      story@Story {..} <- MaybeT $ fromTask <$> getTask nGid
      let url = "<" <> storyUrl projectId story <> ">"
      MaybeT $ do
        logInfo . display $ url <> " " <> sName
        when sCompleted
          . logWarn
          $ "Completed story in iteration: "
          <> display url
        unless (maybe True isFib sCost)
          . logWarn
          $ "Story's cost is not a Fibonacci number: "
          <> display url
        when (isNothing sCost)
          . logWarn
          $ "Story is not costed: "
          <> display url
        case sAssignee of
          Nothing -> do
            logWarn $ "Story has no assignee: " <> display url
            pure Nothing
          Just _ -> mayCanDo story

    let
      isCarried Story {..} = isJust sCarryOver || isJust sCarryOut
      (carriedStories, iterationStories) = partition isCarried stories
      iterationCost = sum $ mapMaybe sCost iterationStories
      iterationNum = length iterationStories
      carriedCost =
        sum $ mapMaybe (\s -> sCarryOver s <|> sCarryOut s) carriedStories
      carriedNum = length carriedStories

    hPutBuilder stdout . getUtf8Builder $ foldMap
      ("\n" <>)
      [ "New Story Points"
      , "  "
      <> display iterationCost
      <> " ("
      <> display iterationNum
      <> " stories)"
      , "Carryover Story Points"
      , "  " <> display carriedCost <> " (" <> display carriedNum <> " stories)"
      , "Total"
      , "  "
      <> display (iterationCost + carriedCost)
      <> " ("
      <> display (iterationNum + carriedNum)
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
      logWarn $ "Story does not have a \"can do?\": " <> display url
      ignoreNoCanDo <- asks $ appIgnoreNoCanDo . appExt
      pure $ if ignoreNoCanDo then Nothing else Just story
    Just canDo -> do
      unless canDo . logWarn $ "Story marked as can't do: " <> display url
      pure $ Just story

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

isFib :: Integer -> Bool
isFib i = elem i $ takeWhile (<= i) fibs
