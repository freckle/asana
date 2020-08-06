module Main (main) where

import RIO

import Asana.Api
import Asana.Api.Gid (Gid)
import Asana.App
import Asana.Story
import Control.Monad (when)
import Data.Char (toLower)
import Data.Maybe (isJust, isNothing)
import Data.Semigroup (Sum(..), (<>))
import Data.Semigroup.Generic (gmappend, gmempty)
import Safe (headMay)
import System.IO (getLine, putStr)

data AppExt = AppExt
  { appProjectId :: Gid
  , appPerspective :: Perspective
  }

main :: IO ()
main = do
  app <- loadAppWith $ AppExt <$> parseProjectId <*> parsePessimistic
  runApp app $ do
    projectId <- asks $ appProjectId . appExt
    perspective <- asks $ appPerspective . appExt
    projectTasks <- getProjectTasks projectId AllTasks

    tasks <- pooledForConcurrentlyN maxRequests projectTasks (getTask . nGid)

    stories <- fmap catMaybes . for tasks $ \task -> do
      let mStory = fromTask task
      for mStory $ \story@Story {..} -> do
        let url = "<" <> storyUrl projectId story <> ">"
        logInfo . display $ sName <> " " <> url

        let
          incompleteNoCarry =
            not sCompleted
              && (isNothing sCarryOver && isNothing sCarryOut)
              && maybe True (> 0) sCost

        when incompleteNoCarry
          $ logWarn
          $ "No carry over on incomplete story: "
          <> display url

        pure $ case (incompleteNoCarry, perspective) of
          (True, Pessimistic) -> story { sCarryOver = sCost }
          (_, _) -> story

    let capitalizedStats = statStories $ filter sCapitalized stories

    hPutBuilder stdout $ getUtf8Builder $ foldMap
      ("\n" <>)
      [ "Capitalized"
      , "- "
      <> display (getSum $ completed capitalizedStats)
      <> " / "
      <> display (getSum $ commitment capitalizedStats)
      ]
    printStats $ statStories stories

    shouldUpdateCompletedPoints <- promptWith
      readBool
      "Update completed points? (y/N)"

    if shouldUpdateCompletedPoints
      then updateCompletedPoints projectId tasks
      else logInfo "Skipping completed point update"

updateCompletedPoints :: Gid -> [Task] -> AppM AppExt ()
updateCompletedPoints projectId tasks =
  pooledForConcurrentlyN_ maxRequests tasks $ \task -> do
    let mStory = fromTask task
    for_ mStory $ \story@(Story {..}) -> do
      let
        mCompletedPoints = case (sCompleted, sCarryIn, sCarryOut) of
          (True, Nothing, _) -> sCost
          (True, Just carryIn, _) -> Just carryIn
          (False, _, Just carryOut) -> (-) <$> sCost <*> Just carryOut
          _ -> Nothing

      case mCompletedPoints of
        Nothing ->
          logWarn
            . display
            $ "Could not update 'points completed' for story <"
            <> storyUrl projectId story
            <> ">"
        Just completedPoints -> do
          let mPointsCompletedField = extractPointsCompletedField task
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
              _ -> logWarn "'points completed' field has incorrect type."

printStats :: MonadIO m => CompletionStats -> m ()
printStats stats@CompletionStats {..} =
  hPutBuilder stdout $ getUtf8Builder $ foldMap
    ("\n" <>)
    [ "Completed"
    , "- new points: " <> display (getSum completedNewCost)
    , "- carried over points: " <> display (getSum completedCarryOver)
    , "- new stories: " <> display (getSum completedNewCount)
    , "- carried over stories: " <> display (getSum carriedCount)
    , "Incomplete"
    , "- points completed: "
      <> display (getSum $ incompleteCost - incompleteCarryOver)
    , "- carry over points: " <> display (getSum incompleteCarryOver)
    , "- carry over stories: " <> display (getSum incompleteCount)
    , ""
    , display (getSum $ completed stats) <> " / " <> display
      (getSum $ commitment stats)
    , "\n"
    ]

completed :: CompletionStats -> Sum Integer
completed CompletionStats {..} =
  completedNewCost + completedCarryOver + (incompleteCost - incompleteCarryOver)

commitment :: CompletionStats -> Sum Integer
commitment CompletionStats {..} =
  completedNewCost + completedCarryOver + incompleteCost

statStories :: [Story] -> CompletionStats
statStories = foldMap storyStats1

storyStats1 :: Story -> CompletionStats
storyStats1 Story {..} = CompletionStats
  { completedNewCost = isNew && sCompleted `implies` cost
  , completedCarryOver = wasCarried && sCompleted `implies` carryIn
  , incompleteCost = not sCompleted `implies` remainingCost
  , incompleteCarryOver = carryOut
  , completedNewCount = sCompleted && isNew `implies` 1
  , carriedCount = willCarry `implies` 1
  , incompleteCount = not sCompleted `implies` 1
  }
 where
  isNew = isNothing sCarryIn
  wasCarried = not isNew
  willCarry = isJust sCarryOut
  cost = maybe mempty Sum sCost
  carryIn = maybe mempty Sum sCarryIn
  carryOut = maybe mempty Sum sCarryOut
  remainingCost = maybe mempty Sum $ (-) <$> sCost <*> sCarryOut

data CompletionStats = CompletionStats
  { completedNewCount :: Sum Int
  , carriedCount :: Sum Int
  , incompleteCount :: Sum Int
  , completedNewCost :: Sum Integer
  , completedCarryOver :: Sum Integer
  , incompleteCost :: Sum Integer
  , incompleteCarryOver :: Sum Integer
  } deriving (Generic)

instance Semigroup CompletionStats where
  (<>) = gmappend

instance Monoid CompletionStats where
  mempty = gmempty

promptWith :: MonadIO m => (String -> b) -> String -> m b
promptWith readVar var = liftIO $ do
  putStr $ var <> ": "
  hFlush stdout
  readVar <$> getLine

readBool :: String -> Bool
readBool str = case map toLower str of
  'y' : _ -> True
  _ -> False

extractPointsCompletedField :: Task -> Maybe CustomField
extractPointsCompletedField Task {..} =
  headMay $ flip mapMaybe tCustomFields $ \case
    customField@(CustomNumber _ "points completed" _) -> Just customField
    _ -> Nothing

infixl 1 `implies`
implies :: Monoid m => Bool -> m -> m
implies predicate a = if predicate then a else mempty
