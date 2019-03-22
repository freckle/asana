{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Asana.App
  ( App(..)
  , AppM
  , Perspective(..)
  , loadApp
  ) where

import Prelude

import Control.Monad.Logger
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import FrontRow.App
import qualified FrontRow.App.Env as Env
import Options.Applicative

data App = App
  { appProjectId :: Text
  , appApiAccessKey :: Text
  , appLogLevel :: LogLevel
  , appPerspective :: Perspective
  , appIgnoreNoCanDo :: Bool
  }

data Perspective = Pessimistic | Optimistic

instance HasLogLevel App where
  getLogLevel = appLogLevel

type AppM = ReaderT App (LoggingT IO)

loadApp :: IO App
loadApp = do
  appApiAccessKey <- Env.parse envParser
  (appProjectId, appLogLevel, appPerspective, appIgnoreNoCanDo) <-
    execParser $ info (helper <*> optParser) $ fullDesc <> progDesc
      "Report information about an iteration project"
  pure App {..}
 where
  envParser :: Env.Parser Text
  envParser = Env.var Env.str "ASANA_API_KEY" Env.nonEmpty

  optParser :: Parser (Text, LogLevel, Perspective, Bool)
  optParser =
    (,,,)
      <$> (T.pack <$> strOption (long "project" <> help "Project Id"))
      <*> flag LevelInfo LevelDebug (long "debug")
      <*> flag Optimistic Pessimistic (long "pessimistic")
      <*> flag False True (long "ignore-no-can-do")
