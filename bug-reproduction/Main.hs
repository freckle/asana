module Main (main) where

import Asana.Prelude

import Asana.Api.Gid (Gid)
import Asana.Api.Named
import Asana.Api.Task
import Asana.App
import Asana.Story
import Data.Semigroup.Generic (gmappend, gmempty)
import Data.Time

newtype AppExt = AppExt
  { appProjectId :: Gid
  }

data Totals = Totals
  { tUnknown :: [Story]
  , tReproduced :: [Story]
  , tNotReproduced :: [Story]
  }
  deriving stock Generic

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
  app <- loadAppWith $ AppExt <$> parseProjectId
  runApp app $ do
    projectId <- asks $ appProjectId . appExt
    tasks <- getProjectTasksCompletedSince projectId
      $ UTCTime (fromGregorian 2018 10 15) 0

    let
      accumulateStory :: Totals -> Named -> AppM ext Totals
      accumulateStory totals named = do
        mStory <- fromTask Nothing <$> getTask (nGid named)
        case mStory of
          Nothing -> pure mempty
          Just story@Story {..} -> do
            storyTotals <- case sAssignee of
              -- Only report assigned tasks; a cheap way to avoid non-bugs that were
              -- marked completed to make them go away.
              Just assignee | sCompleted -> do
                when (isNothing sReproduced)
                  $ logWarn
                  $ "Story lacks reproduced information"
                  :# [ "story" .= sName
                     , "assignee" .= nName assignee
                     , "completedAt" .= sCompletedAt
                     ]
                pure $ toTotals story
              _ -> pure mempty

            pure $ totals <> storyTotals

    totals <- foldM accumulateStory mempty tasks
    liftIO $ putStrLn $ foldMap
      ("\n" <>)
      [ "Unknown: " <> show (length $ tUnknown totals)
      , "Reproduced: " <> show (length $ tReproduced totals)
      , "Not reproduced: " <> show (length $ tNotReproduced totals)
      ]
