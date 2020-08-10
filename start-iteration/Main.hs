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
    projectTasks <- getProjectTasks projectId AllTasks

    tasks <- pooledForConcurrentlyN maxRequests projectTasks (getTask . nGid)

    shouldUpdateCommitment <- promptWith readBool "Update commitment? (y/N)"
    shouldMoveCarryOut <- promptWith
      readBool
      "Move carry out to carry in? (y/N)"

    if shouldUpdateCommitment
      then pooledForConcurrentlyN_
        maxRequests
        tasks
        (updateCommitment projectId)
      else logInfo "Skipping commitment update"

    if shouldMoveCarryOut
      then pooledForConcurrentlyN_ maxRequests tasks (moveCarryOutIn projectId)
      else logInfo "Skipping carry out -> carry in move"

    -- Re-fetch tasks after making changes to commitment/carry in
    tasks1 <- pooledForConcurrentlyN maxRequests projectTasks (getTask . nGid)
    stories <- fmap catMaybes . for tasks1 $ \task -> runMaybeT $ do
      story@Story {..} <- MaybeT $ pure $ fromTask task
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
        when (isJust sCarryOut)
          . logWarn
          $ "Story still has 'carry out' (did you mean to move to 'carry in'?): "
          <> display url
        when (isNothing sCommitment)
          . logWarn
          $ "Story has no 'commitment' (did you mean to update it?): "
          <> display url
        case sAssignee of
          Nothing -> do
            logWarn $ "Story has no assignee: " <> display url
            pure Nothing
          Just _ -> mayCanDo story

    let
      isCarried = isJust . sCarryIn
      (carriedStories, iterationStories) = partition isCarried stories
      iterationCost = sum $ mapMaybe sCost iterationStories
      iterationNum = length iterationStories
      carriedCost = sum $ mapMaybe sCarryIn carriedStories
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

-- | Calculate commitment for this iteration and update the @commitment@ field
updateCommitment :: Gid -> Task -> AppM AppExt ()
updateCommitment projectId task = do
  let mStory = fromTask task
  for_ mStory $ \story@Story {..} -> do
    let
      mCommitment = sCarryIn <|> sCost
      mCommitmentField = extractNumberField "commitment" task

    case mCommitmentField of
      Nothing ->
        logWarn
          . display
          $ "No 'commitment' field for story "
          <> storyUrl projectId story
          <> ">. Skipping."
      Just commitmentField -> case commitmentField of
        CustomNumber gid t _ -> do
          putCustomField
            (tGid task)
            (CustomNumber gid t (fromInteger <$> mCommitment))
          logInfo
            . display
            $ "Updated 'commitment' for story "
            <> display (storyUrl projectId story)
            <> " to "
            <> displayShow mCommitment
        _ -> error "impossible"

-- | Move @carry out@ to @carry in@ if it is set.
moveCarryOutIn :: Gid -> Task -> AppM AppExt ()
moveCarryOutIn projectId task = do
  let mStory = fromTask task
  for_ mStory $ \story@Story {..} -> case sCarryOut of
    Nothing ->
      logInfo
        . display
        $ "No 'carry out' for story "
        <> makeUrl projectId story
        <> ". Skipping copy."
    Just carryOut -> do
      let mCarryInField = extractNumberField "carry in" task
      let mCarryOutField = extractNumberField "carry in" task

      case mCarryInField of
        Nothing ->
          logWarn
            . display
            $ "No 'carry in' field for story "
            <> makeUrl projectId story
            <> ". Skipping copy."
        Just (CustomNumber gid t _) -> do
          putCustomField
            (tGid task)
            (CustomNumber gid t (Just $ fromInteger carryOut))

          -- Wipe @carry out@
          case mCarryOutField of
            Just (CustomNumber outGid outT _) ->
              putCustomField (tGid task) (CustomNumber outGid outT Nothing)
            _ -> error "impossible"

          logInfo
            . display
            $ "Updated 'carry in' for story "
            <> makeUrl projectId story
            <> " to "
            <> tshow carryOut
        Just _ -> error "impossible"

makeUrl :: Gid -> Story -> Text
makeUrl projectId story = "<" <> storyUrl projectId story <> ">"

mayCanDo :: Story -> AppM AppExt (Maybe Story)
mayCanDo story = do
  projectId <- asks $ appProjectId . appExt
  let url = makeUrl projectId story
  case sCanDo story of
    Nothing -> do
      logWarn $ "Story does not have a 'can do?': " <> display url
      ignoreNoCanDo <- asks $ appIgnoreNoCanDo . appExt
      pure $ if ignoreNoCanDo then Nothing else Just story
    Just canDo -> do
      unless canDo . logWarn $ "Story marked as can't do: " <> display url
      pure $ Just story

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

isFib :: Integer -> Bool
isFib i = elem i $ takeWhile (<= i) fibs
