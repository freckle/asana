module Main (main) where

import Asana.Prelude

import Asana.Api.CustomField
import Asana.Api.Gid
import Asana.Api.Task
import Asana.App
import Data.List (find)
import qualified Data.Text as T
import Options.Applicative

main :: IO ()
main = do
  app <- loadAppWith parseOptions

  runApp app $ do
    Options {..} <- asks appExt
    customFields <- tCustomFields <$> getTask oTaskId
    putCustomFields oTaskId $ CustomFields $ mapMaybe
      (override customFields)
      oSets

data Options = Options
  { oSets :: [Set]
  , oTaskId :: Gid
  }

parseOptions :: Parser Options
parseOptions =
  Options
    <$> many (option (eitherReader readSet) (long "set"))
    <*> (textToGid . pack <$> strArgument (metavar "TASK_ID"))

data Set = Set Field Text

newtype Field = Custom Text

readSet :: String -> Either String Set
readSet = go . pack
 where
  go x = case T.breakOn ":" x of
    (k, v) | not (T.null v) -> Right $ Set (Custom k) $ T.drop 1 v
    _ -> Left "Invalid filter, must be <field>:[value]"

override :: CustomFields -> Set -> Maybe CustomField
override fields (Set (Custom name) val) = do
  field <- find (named name) (getCustomFields fields)

  case field of
    CustomNumber gid _ _ -> do
      n <- readMaybe $ unpack val
      Just $ CustomNumber gid name $ Just n
    CustomEnum gid _ opts _ -> Just $ CustomEnum gid name opts $ Just val
    CustomText gid _ _ -> Just $ CustomText gid name $ Just val
    Other -> Nothing

named :: Text -> CustomField -> Bool
named name = \case
  CustomNumber _ n _ -> name == n
  CustomEnum _ n _ _ -> name == n
  CustomText _ n _ -> name == n
  Other -> False
