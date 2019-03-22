module Main (main) where

import Prelude

import Asana.Api
import Asana.App
import Asana.Story
import Data.Maybe (isNothing)
import Data.Semigroup.Generic (gmappend, gmempty)
import Data.Text (pack)
import Data.Time
import FrontRow.App
import GHC.Generics

data Totals = Totals
  { tUnknown :: [Story]
  , tReproduced :: [Story]
  , tNotReproduced :: [Story]
  }
  deriving (Show, Generic)

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
            when (isNothing sReproduced) $ logWarnN $ pack $ unlines
              [ "Story " <> show sName
              , " by " <> show (nName assignee)
              , " completed " <> show sCompletedAt
              , " lacks reproduced information"
              ]
            pure $ toTotals story
          _ -> pure mempty

        pure $ totals <> storyTotals

    totals <- foldM accumulateStory mempty tasks
    liftIO $ putStrLn $ unlines
      [ "Unknown: " <> show (length $ tUnknown totals)
      , "Reproduced: " <> show (length $ tReproduced totals)
      , "Not reproduced: " <> show (length $ tNotReproduced totals)
      ]
