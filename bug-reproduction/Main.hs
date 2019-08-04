module Main (main) where

import RIO

import Asana.Api
import Asana.App
import Asana.Story
import Control.Monad (foldM, when)
import Data.Maybe (isNothing)
import Data.Semigroup.Generic (gmappend, gmempty)
import RIO.Time

data Totals = Totals
  { tUnknown :: [Story]
  , tReproduced :: [Story]
  , tNotReproduced :: [Story]
  }
  deriving (Generic)

instance Semigroup Totals where
  (<>) = gmappend

instance Monoid Totals where
  mappend = (<>)
  mempty = gmempty

toTotals :: Story -> Totals
toTotals story@Story {..}
  | sReproduced == Just True = Totals [] [story] []
  | sReproduced == Just False = Totals [] [] [story]
  | otherwise = Totals [story] [] []

main :: IO ()
main = do
  app <- loadApp
  runApp app $ do
    projectId <- asks appProjectId
    tasks <- getProjectTasksCompletedSince projectId
      $ UTCTime (fromGregorian 2018 10 15) 0

    let
      accumulateStory :: Totals -> Named -> AppM Totals
      accumulateStory totals named = do
        story@Story {..} <- fromTask <$> getTask (nId named)
        storyTotals <- case sAssignee of
          -- Only report assigned tasks; a cheap way to avoid non-bugs that were
          -- marked completed to make them go away.
          Just assignee | sCompleted -> do
            when (isNothing sReproduced) $ logWarn $ foldMap
              ("\n" <>)
              [ "Story " <> display sName
              , " by " <> display (nName assignee)
              , " completed " <> displayShow sCompletedAt
              , " lacks reproduced information"
              ]
            pure $ toTotals story
          _ -> pure mempty

        pure $ totals <> storyTotals

    totals <- foldM accumulateStory mempty tasks
    hPutBuilder stdout . getUtf8Builder $ foldMap
      ("\n" <>)
      [ "Unknown: " <> display (length $ tUnknown totals)
      , "Reproduced: " <> display (length $ tReproduced totals)
      , "Not reproduced: " <> display (length $ tNotReproduced totals)
      ]
