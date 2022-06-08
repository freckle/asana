{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Asana.Prelude

import Asana.Api.CustomField
import Asana.Api.Gid (Gid)
import Asana.Api.Named
import Asana.Api.Request
import Asana.Api.Task
import Asana.App
import Asana.Story
import Data.Semigroup (Sum(..))
import Data.Semigroup.Generic (gmappend, gmempty)
import qualified Data.Text as T
import UnliftIO.Async (pooledForConcurrentlyN, pooledForConcurrentlyN_)

data AppExt = AppExt
  { appProjectId :: Gid
  , appSubprojectName :: Maybe String
  }

main :: IO ()
main = do
  app <- loadAppWith $ AppExt <$> parseProjectId <*> parseSubprojectName

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

    stories <- fmap catMaybes . for tasks $ \task -> do
      let mStory = fromTask (Just projectId) task
      for mStory $ \story@Story {..} -> do
        let url = storyUrl projectId story

        logInfo $ sName :# ["url" .= url, "status" .= getStoryStatus story]

        let
          incompleteNoCarry =
            not sCompleted && isNothing sCarryOut && maybe True (> 0) sCost
          doubleCarry = not sCompleted && isJust sCarryIn


        when incompleteNoCarry
          $ logWarn
          $ "No carry over on incomplete story"
          :# ["url" .= url]

        when doubleCarry
          $ logError
          $ "Story is going to carry twice"
          :# ["url" .= url]

        pure story

    let
      capitalizedStats = statStories $ filter sCapitalized stories
      fullStats = statStories stories

    liftIO $ putStrLn $ foldMap
      ("\n" <>)
      [ "Capitalized"
      , "- " <> show (getSum $ completed capitalizedStats) <> " / " <> show
        (getSum $ completed fullStats)
      ]
    printStats fullStats

    shouldUpdateCompletedPoints <- promptWith
      readBool
      "Update completed points? (y/N)"

    if shouldUpdateCompletedPoints
      then updateCompletedPoints projectId tasks
      else logInfo "Skipping completed point update"

getCompletedPoints :: Story -> Maybe Integer
getCompletedPoints Story {..} = case (sCompleted, sCarryIn, sCarryOut) of
  (True, Nothing, _) -> sCost
  (True, Just carryIn, _) -> Just carryIn
  (False, _, Just carryOut) -> subtract carryOut <$> sCost
  _ -> Nothing

updateCompletedPoints :: Gid -> [Task] -> AppM AppExt ()
updateCompletedPoints projectId tasks =
  pooledForConcurrentlyN_ maxRequests tasks $ \task -> do
    let mStory = fromTask (Just projectId) task
    for_ mStory $ \story -> case getCompletedPoints story of
      Nothing ->
        logWarn
          $ "Could not update 'points completed'"
          :# ["url" .= storyUrl projectId story]
      Just completedPoints -> do
        let mPointsCompletedField = extractNumberField "points completed" task
        case mPointsCompletedField of
          Nothing ->
            logWarn
              $ "Skipping due to no 'points completed' field"
              :# ["url" .= storyUrl projectId story]
          Just pointsCompletedField -> case pointsCompletedField of
            CustomNumber gid t _ -> do
              putCustomField
                (tGid task)
                (CustomNumber gid t (Just $ fromInteger completedPoints))
              logInfo
                $ "Updated 'points completed'"
                :# [ "url" .= storyUrl projectId story
                   , "points" .= completedPoints
                   ]
            _ -> error "impossible"

printStats :: MonadIO m => CompletionStats -> m ()
printStats stats@CompletionStats {..} = liftIO $ putStrLn $ foldMap
  ("\n" <>)
  [ "Completed Task Stats"
  , "- new points: " <> show (getSum completedNewCost)
  , "- carried over points: " <> show (getSum completedCarryOver)
  , "- new stories: " <> show (getSum completedNewCount)
  , "- carried over stories: " <> show (getSum carriedCount)
  , "Incomplete Task Stats"
  , "- points completed: " <> show (getSum incompleteCompletedPoints)
  , "- carry over points: " <> show (getSum incompleteCarryOver)
  , "- carry over stories: " <> show (getSum incompleteCount)
  , ""
  , show (getSum $ completed stats) <> " / " <> show (getSum commitment)
  , "\n"
  ]

completed :: CompletionStats -> Sum Integer
completed CompletionStats {..} =
  completedNewCost + completedCarryOver + incompleteCompletedPoints

statStories :: [Story] -> CompletionStats
statStories = foldMap storyStats1

storyStats1 :: Story -> CompletionStats
storyStats1 story@Story {..} = CompletionStats
  { completedNewCost = isNew && sCompleted `implies` completedPoints
  , completedCarryOver = wasCarried && sCompleted `implies` completedPoints
  , incompleteCompletedPoints = not sCompleted `implies` completedPoints
  , incompleteCarryOver = carryOut
  , completedNewCount = sCompleted && isNew `implies` 1
  , carriedCount = willCarry `implies` 1
  , incompleteCount = not sCompleted `implies` 1
  , commitment
  }
 where
  isNew = isNothing sCarryIn
  wasCarried = not isNew
  willCarry = isJust sCarryOut
  carryOut = maybe mempty Sum sCarryOut
  commitment = maybe mempty Sum sCommitment
  completedPoints = maybe mempty Sum $ getCompletedPoints story

data CompletionStats = CompletionStats
  { completedNewCount :: Sum Int
  , carriedCount :: Sum Int
  , incompleteCount :: Sum Int
  , completedNewCost :: Sum Integer
  , completedCarryOver :: Sum Integer
  , incompleteCompletedPoints :: Sum Integer
  , incompleteCarryOver :: Sum Integer
  , commitment :: Sum Integer
  }
  deriving stock Generic

instance Semigroup CompletionStats where
  (<>) = gmappend

instance Monoid CompletionStats where
  mempty = gmempty

infixl 1 `implies`
implies :: Monoid m => Bool -> m -> m
implies predicate a = if predicate then a else mempty

getStoryStatus :: Story -> Text
getStoryStatus Story {..}
  | sCompleted && isJust sCarryIn = "Completed carry"
  | sCompleted && isNothing sCarryIn = "Completed new"
  | not sCompleted && isJust sCarryOut = "Incomplete"
  | otherwise = "Unknown"
