module Asana.Story
  ( Story(..)
  , fromTask
  , storyUrl
  ) where

import RIO

import Asana.Api
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Semigroup ((<>))
import RIO.Text (Text)
import qualified RIO.Text as T
import RIO.Time

data Story = Story
  { sAssignee :: Maybe Named
  , sName :: Text
  , sCompleted :: Bool
  , sCompletedAt :: Maybe UTCTime
  , sCost :: Maybe Integer
  , sVirality :: Maybe Integer
  , sImpact :: Maybe Integer
  , sCarryOver :: Maybe Integer
  , sCanDo :: Maybe Bool
  , sReproduced :: Maybe Bool
  , sId :: Int
  }
  deriving Show

fromTask :: Task -> Maybe Story
fromTask Task {..} = case tResourceSubtype of
  Milestone -> Nothing
  Section -> Nothing
  DefaultTask -> Just $ Story
    { sAssignee = tAssignee
    , sName = tName
    , sCompleted = tCompleted || awaitingDeployment
    , sCompletedAt = tCompletedAt
    , sCost = findNumber "cost" tCustomFields
    , sImpact = findNumber "impact" tCustomFields
    , sVirality = findNumber "virality" tCustomFields
    , sCarryOver = findNumber "carryover" tCustomFields
    , sCanDo = findYesNo "can do?" tCustomFields
    , sReproduced = findYesNo "Reproduces on seed data?" tCustomFields
    , sId = tId
    }
 where
  awaitingDeployment = flip any tMemberships $ \Membership {..} ->
    case mSection of
      Just Named {..} | caseFoldEq nName "Awaiting Deployment" -> True
      _ -> False

findNumber :: Text -> [CustomField] -> Maybe Integer
findNumber field = listToMaybe . mapMaybe cost
 where
  cost = \case
    (CustomNumber foundField mn) | caseFoldEq field foundField -> mn
    _ -> Nothing

findYesNo :: Text -> [CustomField] -> Maybe Bool
findYesNo x = fmap parse . listToMaybe . mapMaybe go
 where
  go (CustomEnum name mn) | caseFoldEq name x = mn
  go _ = Nothing
  parse = (== "Yes")

caseFoldEq :: Text -> Text -> Bool
caseFoldEq x y = T.toCaseFold x == T.toCaseFold y

storyUrl :: Text -> Story -> Text
storyUrl projectId Story {..} =
  "https://app.asana.com/0/" <> projectId <> "/" <> T.pack (show sId) <> "/f"
