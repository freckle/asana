{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Asana.App
  ( App(..)
  , AppM
  , Perspective(..)
  , loadApp
  , runApp
  , liftIO
  ) where

import RIO

import Asana.Api.Gid (Gid, textToGid)
import Control.Monad.IO.Class (liftIO)
import Data.Semigroup ((<>))
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
import RIO.Text (Text)
import qualified RIO.Text as T
import System.Environment (getEnv)
import System.IO (stderr)

data App = App
  { appProjectId :: Gid
  , appApiAccessKey :: Text
  , appLogLevel :: LogLevel
  , appPerspective :: Perspective
  , appIgnoreNoCanDo :: Bool
  , logFunc :: LogFunc
  }

instance HasLogFunc App where
  logFuncL = lens logFunc (\app logFunc -> app {logFunc})

data Perspective = Pessimistic | Optimistic

type AppM = RIO App

runApp :: App -> AppM a -> IO a
runApp app action = do
  logOptions <-
    setLogUseLoc False
    . setLogUseTime False
    . setLogMinLevel (appLogLevel app)
    <$> logOptionsHandle stderr True
  withLogFunc logOptions $ \logFunc -> runRIO app { logFunc } $ do
    logInfo "Starting app"
    action

loadApp :: IO App
loadApp = do
  loadEnvFrom ".env.asana"
  appApiAccessKey <- T.pack <$> getEnv "ASANA_API_KEY"
  (appProjectId, appLogLevel, appPerspective, appIgnoreNoCanDo) <-
    execParser $ info (helper <*> optParser) $ fullDesc <> progDesc
      "Report information about an iteration project"
  let logFunc = error "not initialized"
  pure App { .. }
 where
  optParser :: Parser (Gid, LogLevel, Perspective, Bool)
  optParser =
    (,,,)
      <$> (textToGid . T.pack <$> strOption
            (long "project" <> help "Project Id")
          )
      <*> flag LevelInfo LevelDebug (long "debug")
      <*> flag Optimistic Pessimistic (long "pessimistic")
      <*> flag False True (long "ignore-no-can-do")
