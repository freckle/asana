{-# LANGUAGE NamedFieldPuns #-}
module Asana.Story
  ( Story(..)
  , fromTask
  , storyUrl
  ) where

import Asana.Prelude

import Asana.Api.CustomField
import Asana.Api.Gid (Gid, gidToText)
import Asana.Api.Named
import Asana.Api.Task
import Control.Applicative ((<|>))
import Data.Scientific (Scientific)
import qualified Data.Text as T
import Data.Time (UTCTime)

data Story = Story
  { sAssignee :: Maybe Named
  , sName :: Text
  , sCompleted :: Bool
  , sCompletedAt :: Maybe UTCTime
  , sCost :: Maybe Integer
  , sCommitment :: Maybe Integer
  , sVirality :: Maybe Integer
  , sImpact :: Maybe Integer
  , sCarryIn :: Maybe Integer
  , sCarryOut :: Maybe Integer
  , sCanDo :: Maybe Bool
  , sReproduced :: Maybe Bool
  , sCapitalized :: Bool
  , sGid :: Gid
  }
  deriving stock Show

fromTask
  :: Maybe Gid
  -- ^ Optional projectId to search for Carryover section within to determine
  -- carry-in vs carry-out. Unnecessary for non-iteration-tracking purposes.
  -> Task
  -> Maybe Story
fromTask mProjectId Task {..} = case tResourceSubtype of
  Milestone -> Nothing
  Section -> Nothing
  DefaultTask -> Just $ Story
    { sAssignee = tAssignee
    , sName = tName
    , sCompleted = tCompleted || awaitingDeployment
    , sCompletedAt = tCompletedAt
    , sCost
    , sCommitment = sCarryIn <|> sCost
    , sImpact = findInteger "impact" tCustomFields
    , sVirality = findInteger "virality" tCustomFields
    , sCanDo = findYesNo "can do?" tCustomFields
    , sReproduced = findYesNo "Reproduces on seed data?" tCustomFields
    , sCapitalized = fromMaybe False $ findYesNo "cap?" tCustomFields
    , sGid = tGid
    , sCarryIn
    , sCarryOut
    }
 where
  awaitingDeployment = flip any tMemberships $ \Membership {..} ->
    case mSection of
      Just Named {..} | caseFoldEq nName "Awaiting Deployment" -> True
      _ -> False
  sCost = findInteger "cost" tCustomFields
  isCarryIn = flip any tMemberships $ \Membership {..} -> case mSection of
    Just Named { nName } -> fromMaybe False $ do
      guard $ "carryover" `T.isInfixOf` T.toLower nName
      for_ mProjectId $ \projectId -> guard $ nGid mProject == projectId
      pure True
    _ -> False

  sCarryIn = do
    carry <- findInteger "carryover" tCustomFields
    carry <$ guard isCarryIn

  sCarryOut = do
    carry <- findInteger "carryover" tCustomFields
    carry <$ guard (not isCarryIn)

findInteger :: Text -> CustomFields -> Maybe Integer
findInteger field = fmap round . findNumber field

findNumber :: Text -> CustomFields -> Maybe Scientific
findNumber field = listToMaybe . mapMaybe cost . getCustomFields
 where
  cost = \case
    (CustomNumber _ foundField mn) | caseFoldEq field foundField -> mn
    _ -> Nothing

findYesNo :: Text -> CustomFields -> Maybe Bool
findYesNo x = fmap parse . listToMaybe . mapMaybe go . getCustomFields
 where
  go (CustomEnum _ name _ mn) | caseFoldEq name x = mn
  go _ = Nothing
  parse str = str == "Yes" || str == "Y"

caseFoldEq :: Text -> Text -> Bool
caseFoldEq x y = T.toCaseFold x == T.toCaseFold y

storyUrl :: Gid -> Story -> Text
storyUrl projectId Story {..} =
  "https://app.asana.com/0/"
    <> gidToText projectId
    <> "/"
    <> gidToText sGid
    <> "/f"
