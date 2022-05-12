{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Asana.App
  ( App
  , AppWith(..)
  , AppM
  , Perspective(..)
  , loadAppWith
  , runApp
  , liftIO
  -- * Argument parsers
  , parseIgnoreNoCanDo
  , parseProjectId
  , parseBugProjectId
  , parseTeamProjectId
  , parseYear
  , parseDate
  , parseImport
  , parseSubprojectName
  -- * Prompts
  , promptWith
  , readBool
  ) where

import RIO

import Asana.Api.Gid (Gid, textToGid)
import Asana.Api.Request (HasAsana(..))
import Data.Char (toLower)
import LoadEnv (loadEnvFrom)
import Options.Applicative
  ( Parser
  , auto
  , execParser
  , flag
  , fullDesc
  , help
  , helper
  , info
  , long
  , option
  , progDesc
  , strOption
  )
import qualified RIO.Text as T
import RIO.Time
import System.Environment (getEnv)
import System.IO (getLine, putStr)

data AppWith ext = App
  { appApiAccessKey :: Text
  , appLogLevel :: LogLevel
  , logFunc :: LogFunc
  , appExt :: ext
  }

type App = AppWith ()

instance HasLogFunc (AppWith ext) where
  logFuncL = lens logFunc (\app logFunc -> app { logFunc })

instance HasAsana (AppWith ext) where
  asanaApiAccessKeyL =
    lens appApiAccessKey $ \app appApiAccessKey -> app { appApiAccessKey }

data Perspective = Pessimistic | Optimistic

type AppM ext = RIO (AppWith ext)

runApp :: AppWith ext -> AppM ext a -> IO a
runApp app action = do
  logOptions <-
    setLogUseLoc False
    . setLogUseTime False
    . setLogMinLevel (appLogLevel app)
    <$> logOptionsHandle stderr True
  withLogFunc logOptions $ \logFunc -> runRIO app { logFunc } $ do
    logInfo "Starting app"
    action

loadAppWith :: forall ext . Parser ext -> IO (AppWith ext)
loadAppWith parseExt = do
  loadEnvFrom ".env.asana"
  appApiAccessKey <- T.pack <$> getEnv "ASANA_API_KEY"
  (appLogLevel, appExt) <-
    execParser $ info (helper <*> optParser) $ fullDesc <> progDesc
      "Report information about an iteration project"
  let logFunc = error "not initialized"
  pure App { .. }
 where
  optParser :: Parser (LogLevel, ext)
  optParser =
    resolveLevel
      <$> flag Nothing (Just LevelDebug) (long "debug")
      <*> flag Nothing (Just LevelWarn) (long "quiet")
      <*> parseExt

  resolveLevel debug quiet ext = (fromMaybe LevelInfo $ quiet <|> debug, ext)

parseIgnoreNoCanDo :: Parser Bool
parseIgnoreNoCanDo = flag False True (long "ignore-no-can-do")

parseProjectId :: Parser Gid
parseProjectId =
  textToGid . T.pack <$> strOption (long "project" <> help "Project Id")

parseBugProjectId :: Parser Gid
parseBugProjectId =
  textToGid . T.pack <$> strOption (long "bug-project" <> help "Bug Project Id")

parseYear :: Parser Integer
parseYear = option auto (long "year" <> help "The year to view")

parseDate :: String -> Parser UTCTime
parseDate name = parseTimeOrError False defaultTimeLocale "%Y-%m-%d"
  <$> strOption (long name <> help "[UTCTime]")

parseImport :: Parser (Maybe FilePath)
parseImport = optional (strOption (long "import" <> help "CSV File to import"))

parseSubprojectName :: Parser (Maybe String)
parseSubprojectName =
  optional (strOption (long "subproject" <> help "Optional subproject name"))

parseTeamProjectId :: Parser Gid
parseTeamProjectId =
  textToGid . T.pack <$> strOption (long "team-project" <> help "SubProject Id")

promptWith :: MonadIO m => (String -> b) -> String -> m b
promptWith readVar var = liftIO $ do
  putStr $ var <> ": "
  hFlush stdout
  readVar <$> getLine

readBool :: String -> Bool
readBool str = case map toLower str of
  'y' : _ -> True
  _ -> False
