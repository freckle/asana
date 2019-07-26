module Main (main) where

import Prelude

import Asana.Api
import Asana.App
import Asana.Story
import Control.Monad (unless, when)
import Control.Monad.Logger
import Data.List (partition)
import Data.Maybe (fromMaybe, isJust, isNothing, mapMaybe)
import Data.Semigroup ((<>))
import UnliftIO.Async (pooledForConcurrentlyN)

main :: IO ()
main = do
  app <- loadApp
  runApp app $ do
    projectId <- asks appProjectId
    tasks <- getProjectTasks projectId AllTasks

    stories <-
      fmap (filterNoCanDo (appIgnoreNoCanDo app))
      . pooledForConcurrentlyN maxRequests tasks
      $ \Named {..} -> do
          story@Story {..} <- fromTask <$> getTask nId
          let url = "<" <> storyUrl projectId story <> ">"
          logInfoN $ url <> " " <> sName
          when sCompleted . logWarnN $ "Completed story in iteration: " <> url
          case sCanDo of
            Nothing -> logWarnN $ "Story does not have a \"can do?\": " <> url
            Just canDo ->
              unless canDo . logWarnN $ "Story marked as can't do: " <> url
          unless (maybe True isFib sCost)
            . logWarnN
            $ "Story's cost is not a Fibonacci number: "
            <> url
          when (isNothing sCost) . logWarnN $ "Story is not costed: " <> url
          pure story

    let
      isCarried = isJust . sCarryOver
      (carriedStories, iterationStories) = partition isCarried stories
      iterationCost = sum $ mapMaybe sCost iterationStories
      iterationNum = length iterationStories
      carriedCost = sum $ mapMaybe sCarryOver carriedStories
      carriedNum = length carriedStories
    liftIO $ putStrLn $ unlines
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

filterNoCanDo :: Bool -> [Story] -> [Story]
filterNoCanDo False = id
filterNoCanDo True = filter (fromMaybe False . sCanDo)

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

isFib :: Integer -> Bool
isFib i = elem i $ takeWhile (<= i) fibs
