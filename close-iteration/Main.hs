{-# LANGUAGE NamedFieldPuns #-}
module Main (main) where

import RIO

import Asana.Api
import Asana.Api.Gid (Gid)
import Asana.App
import Asana.Story
import Data.Semigroup (Sum(..))
import Data.Semigroup.Generic (gmappend, gmempty)
import qualified RIO.Text as T

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
        let
          url = "<" <> storyUrl projectId story <> ">"
          status = "(" <> getStoryStatus story <> ")"
        logInfo . display $ sName <> " " <> url <> " " <> status

        let
          incompleteNoCarry =
            not sCompleted && isNothing sCarryOut && maybe True (> 0) sCost
          doubleCarry = not sCompleted && isJust sCarryIn


        when incompleteNoCarry
          $ logWarn
          $ "No carry over on incomplete story: "
          <> display url

        when doubleCarry
          $ logError
          $ "Story is going to carry twice: "
          <> display url

        pure story

    let
      capitalizedStats = statStories $ filter sCapitalized stories
      fullStats = statStories stories

    hPutBuilder stdout $ getUtf8Builder $ foldMap
      ("\n" <>)
      [ "Capitalized"
      , "- "
      <> display (getSum $ completed capitalizedStats)
      <> " / "
      <> display (getSum $ completed fullStats)
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
          . display
          $ "Could not update 'points completed' for story <"
          <> storyUrl projectId story
          <> ">"
      Just completedPoints -> do
        let mPointsCompletedField = extractNumberField "points completed" task
        case mPointsCompletedField of
          Nothing ->
            logWarn
              . display
              $ "No 'points completed' field for story "
              <> storyUrl projectId story
              <> ">. Skipping."
          Just pointsCompletedField -> case pointsCompletedField of
            CustomNumber gid t _ -> do
              putCustomField
                (tGid task)
                (CustomNumber gid t (Just $ fromInteger completedPoints))
              logInfo
                . display
                $ "Updated 'points completed' for story "
                <> display (storyUrl projectId story)
                <> " to "
                <> display completedPoints
            _ -> error "impossible"

printStats :: MonadIO m => CompletionStats -> m ()
printStats stats@CompletionStats {..} =
  hPutBuilder stdout $ getUtf8Builder $ foldMap
    ("\n" <>)
    [ "Completed Task Stats"
    , "- new points: " <> display (getSum completedNewCost)
    , "- carried over points: " <> display (getSum completedCarryOver)
    , "- new stories: " <> display (getSum completedNewCount)
    , "- carried over stories: " <> display (getSum carriedCount)
    , "Incomplete Task Stats"
    , "- points completed: " <> display (getSum incompleteCompletedPoints)
    , "- carry over points: " <> display (getSum incompleteCarryOver)
    , "- carry over stories: " <> display (getSum incompleteCount)
    , ""
    , display (getSum $ completed stats) <> " / " <> display (getSum commitment)
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
