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

import Asana.Prelude

import Asana.Api.Gid (Gid, textToGid)
import Asana.Api.Request (AsanaAccessKey(..), HasAsanaAccessKey(..))
import Blammo.Logging.LogSettings
import Blammo.Logging.LogSettings.LogLevels (defaultLogLevels, newLogLevels)
import Data.Char (toLower)
import qualified Data.Text as T
import Data.Time
import qualified Env
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
  , optional
  , progDesc
  , strOption
  , (<|>)
  )
import System.IO (hFlush, stdout)

data AppWith ext = App
  { appAsanaAccessKey :: AsanaAccessKey
  , appLogger :: Logger
  , appExt :: ext
  }

type App = AppWith ()

instance HasLogger (AppWith ext) where
  loggerL = lens appLogger $ \x y -> x { appLogger = y }

instance HasAsanaAccessKey (AppWith ext) where
  asanaAccessKeyL =
    lens appAsanaAccessKey $ \app appAsanaAccessKey -> app { appAsanaAccessKey }

data Perspective = Pessimistic | Optimistic

type AppM ext a = ReaderT (AppWith ext) (LoggingT IO) a

runApp :: AppWith ext -> AppM ext a -> IO a
runApp app action = runLoggerLoggingT app $ flip runReaderT app $ do
  logInfo "Starting app"
  action

loadAppWith :: forall ext . Parser ext -> IO (AppWith ext)
loadAppWith parseExt = do
  loadEnvFrom ".env.asana"
  (logSettings, appAsanaAccessKey, appExt) <- loadSettings parseExt
  appLogger <- newLogger logSettings

  pure App { .. }

loadSettings
  :: forall ext . Parser ext -> IO (LogSettings, AsanaAccessKey, ext)
loadSettings parseExt = do
  (mLogLevel, appExt) <-
    execParser $ info (helper <*> optParser) $ fullDesc <> progDesc
      "Report information about an iteration project"

  (accessKey, envLogLevels, logDestination, logFormat) <- Env.parse id envParser

  let
    logLevels = maybe envLogLevels (`newLogLevels` []) mLogLevel
    logSettings =
      setLogSettingsLevels logLevels
        . setLogSettingsDestination logDestination
        . setLogSettingsFormat logFormat
        $ defaultLogSettings

  pure (logSettings, accessKey, appExt)
 where
  envParser
    :: Env.Parser
         Env.Error
         (AsanaAccessKey, LogLevels, LogDestination, LogFormat)
  envParser =
    (,,,)
      <$> (AsanaAccessKey <$> Env.var Env.nonempty "ASANA_API_KEY" mempty)
      <*> Env.var
            (first Env.unread . readLogLevels)
            "LOG_LEVEL"
            (Env.def defaultLogLevels)
      <*> Env.var
            (first Env.unread . readLogDestination)
            "LOG_DESTINATION"
            (Env.def LogDestinationStderr)
      <*> Env.var
            (first Env.unread . readLogFormat)
            "LOG_FORMAT"
            (Env.def LogFormatTerminal)

  optParser :: Parser (Maybe LogLevel, ext)
  optParser =
    (,)
      <$> (flag Nothing (Just LevelDebug) (long "debug")
          <|> flag Nothing (Just LevelWarn) (long "quiet")
          )
      <*> parseExt

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
