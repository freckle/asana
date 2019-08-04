module Main (main) where

import RIO

import Asana.Api
import Asana.App
import Asana.Story
import Control.Monad (unless, when)
import Data.List (partition, tail, zipWith)
import Data.Maybe (fromMaybe, isJust, isNothing, mapMaybe)
import Data.Semigroup ((<>))

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
          logInfo . display $ url <> " " <> sName
          when sCompleted
            . logWarn
            $ "Completed story in iteration: "
            <> display url
          case sCanDo of
            Nothing ->
              logWarn $ "Story does not have a \"can do?\": " <> display url
            Just canDo ->
              unless canDo
                . logWarn
                $ "Story marked as can't do: "
                <> display url
          unless (maybe True isFib sCost)
            . logWarn
            $ "Story's cost is not a Fibonacci number: "
            <> display url
          when (isNothing sCost)
            . logWarn
            $ "Story is not costed: "
            <> display url
          pure story

    let
      isCarried = isJust . sCarryOver
      (carriedStories, iterationStories) = partition isCarried stories
      iterationCost = sum $ mapMaybe sCost iterationStories
      iterationNum = length iterationStories
      carriedCost = sum $ mapMaybe sCarryOver carriedStories
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

filterNoCanDo :: Bool -> [Story] -> [Story]
filterNoCanDo False = id
filterNoCanDo True = filter (fromMaybe False . sCanDo)

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

isFib :: Integer -> Bool
isFib i = elem i $ takeWhile (<= i) fibs
