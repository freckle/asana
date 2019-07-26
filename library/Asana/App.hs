{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Asana.App
  ( App(..)
  , AppM
  , Perspective(..)
  , loadApp
  , runApp
  , asks
  , liftIO
  ) where

import Prelude

import Asana.Logger (runANSILoggerT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger
import Control.Monad.Trans.Reader (ReaderT, asks, runReaderT)
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import LoadEnv
import Options.Applicative
  ( Parser
  , execParser
  , flag
  , fullDesc
  , help
  , helper
  , info
  , long
  , progDesc
  , strOption
  )
import System.Environment (getEnv)
import System.IO (BufferMode(..), hSetBuffering, stderr, stdout)

data App = App
  { appProjectId :: Text
  , appApiAccessKey :: Text
  , appLogLevel :: LogLevel
  , appPerspective :: Perspective
  , appIgnoreNoCanDo :: Bool
  }

data Perspective = Pessimistic | Optimistic

type AppM = ReaderT App (LoggingT IO)

runApp :: App -> AppM a -> IO a
runApp app action = do
  -- Ensure output is streamed if in a Docker container
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  runANSILoggerT
    $ filterLogger (\_ level -> level >= appLogLevel app)
    $ runReaderT action app

loadApp :: IO App
loadApp = do
  loadEnvFrom ".env.asana"
  appApiAccessKey <- T.pack <$> getEnv "ASANA_API_KEY"
  (appProjectId, appLogLevel, appPerspective, appIgnoreNoCanDo) <-
    execParser $ info (helper <*> optParser) $ fullDesc <> progDesc
      "Report information about an iteration project"
  pure App {..}
 where
  optParser :: Parser (Text, LogLevel, Perspective, Bool)
  optParser =
    (,,,)
      <$> (T.pack <$> strOption (long "project" <> help "Project Id"))
      <*> flag LevelInfo LevelDebug (long "debug")
      <*> flag Optimistic Pessimistic (long "pessimistic")
      <*> flag False True (long "ignore-no-can-do")
