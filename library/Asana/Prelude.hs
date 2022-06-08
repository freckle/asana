module Asana.Prelude
  ( module X
  , tshow
  ) where

import Prelude as X

import Blammo.Logging as X
import Control.Arrow as X ((&&&), (***))
import Control.Lens as X (Lens', lens, view)
import Control.Monad.IO.Unlift as X (MonadUnliftIO)
import Control.Monad.Reader as X
import Data.Bifunctor as X (first, second)
import Data.ByteString as X (ByteString)
import Data.Foldable as X (for_)
import Data.Maybe as X
  (catMaybes, fromMaybe, isJust, isNothing, listToMaybe, mapMaybe)
import Data.Text as X (Text, pack, unpack)
import Data.Traversable as X (for)
import GHC.Generics as X (Generic)
import Text.Read as X (readMaybe)
import UnliftIO.Exception as X (Exception(..), catch, throwIO)

tshow :: Show a => a -> Text
tshow = pack . show

{-# ANN module ("HLint: avoid restricted qualification" :: String) #-}
