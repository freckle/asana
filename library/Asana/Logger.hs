module Asana.Logger
  ( runANSILoggerT
  ) where

import Prelude

import Control.Monad.Logger (LogLevel(..), LoggingT, defaultLogStr, runLoggingT)
import qualified Data.ByteString as BS
import System.Console.ANSI
  ( Color(Blue, Magenta, Red, Yellow)
  , ColorIntensity(Dull)
  , ConsoleLayer(Foreground)
  , SGR(Reset, SetColor)
  , hSetSGR
  , setSGR
  )
import System.IO (Handle, stderr, stdout)
import System.Log.FastLogger (fromLogStr)

runANSILoggerT :: LoggingT m a -> m a
runANSILoggerT = (`runLoggingT` logger)
 where
  labelEnd = fromIntegral $ fromEnum ']'
  logger loc src level str = do
    let
      h = levelHandle level
      (levelStr : logStr) =
        BS.split labelEnd . fromLogStr $ defaultLogStr loc src level str
    hSetSGR h [style level]
    BS.hPutStr h $ BS.snoc levelStr labelEnd
    hSetSGR h [Reset]
    BS.hPutStr h $ BS.intercalate (BS.singleton labelEnd) logStr
    setSGR [Reset]

style :: LogLevel -> SGR
style = \case
  LevelDebug -> SetColor Foreground Dull Magenta
  LevelInfo -> SetColor Foreground Dull Blue
  LevelWarn -> SetColor Foreground Dull Yellow
  LevelError -> SetColor Foreground Dull Red
  LevelOther _ -> Reset

levelHandle :: LogLevel -> Handle
levelHandle = \case
  LevelDebug -> stderr
  LevelInfo -> stdout
  LevelWarn -> stderr
  LevelError -> stderr
  LevelOther _ -> stdout
