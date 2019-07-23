module Asana.Api.Request
  ( Single(..)
  , Page(..)
  , NextPage(..)
  , getAll
  , getAllParams
  , getSingle
  , put
  , maxRequests
  ) where

import Prelude

import Asana.App
import Control.Monad.Logger (logWarnN)
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Casing
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import GHC.Generics
import Network.HTTP.Client.Conduit
  ( HttpExceptionContent(ConnectionTimeout, ResponseTimeout, StatusCodeException)
  )
import Network.HTTP.Simple
import Text.Read (readMaybe)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (Exception, Handler(Handler), catches, throwIO)

maxRequests :: Int
maxRequests = 50

-- | Type for a single-resource response, containing @{ data: { ... } }@
newtype Single a = Single
  { sData :: a
  }
  deriving (Eq, Generic, Show)

instance FromJSON a => FromJSON (Single a) where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

-- | Type for a list-resource response, containing @{ data: [{ ... }] }@
data Page a = Page
  { pData :: [a]
  , pNextPage :: Maybe NextPage
  }
  deriving (Eq, Generic, Show)

instance FromJSON a => FromJSON (Page a) where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

-- | The @next_page@ element of a paginated response
data NextPage = NextPage
  { npOffset :: Text
  , npPath :: Text
  , npUri :: Text
  }
  deriving (Eq, Generic, Show)

instance FromJSON NextPage where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

-- | Naively GET all pages of a paginated resource
getAll :: FromJSON a => String -> AppM [a]
getAll path = getAllParams path []

getAllParams :: FromJSON a => String -> [(String, String)] -> AppM [a]
getAllParams path params = go Nothing
 where
  go mOffset = do
    Page d mNextPage <- get path params 50 mOffset

    maybe (pure d) (fmap (d ++) . go . Just . T.unpack . npOffset) mNextPage

-- | Get a single resource
getSingle :: FromJSON a => String -> AppM a
getSingle path = sData <$> get path [] 1 Nothing

get
  :: FromJSON a => String -> [(String, String)] -> Int -> Maybe String -> AppM a
get path params limit mOffset = do
  auth <- asks appApiAccessKey
  request <-
    parseRequest
    $ "https://app.asana.com/api/1.0"
    <> path
    <> "?limit="
    <> show limit -- Ignored on not paging responses
    <> maybe "" ("&offset=" <>) mOffset
    <> concatMap (\(k, v) -> "&" <> k <> "=" <> v) params
  response <- retry 10 $ httpJSON (addAuthorization auth request)
  when (300 <= getResponseStatusCode response)
    . logWarnN
    . T.pack
    $ "GET failed "
    <> show (getResponseStatusCode response)
  pure $ getResponseBody response

put :: ToJSON a => String -> a -> AppM ()
put path payload = do
  auth <- asks appApiAccessKey
  request <- parseRequest $ "https://app.asana.com/api/1.0" <> path

  response <- retry 10 $ httpNoBody
    (setRequestMethod "PUT" . setRequestBodyJSON payload $ addAuthorization
      auth
      request
    )
  when (300 <= getResponseStatusCode response)
    . logWarnN
    . T.pack
    $ "PUT failed "
    <> show (getResponseStatusCode response)

addAuthorization :: Text -> Request -> Request
addAuthorization auth =
  addRequestHeader "Authorization" $ "Bearer " <> encodeUtf8 auth

retry :: forall a . Int -> AppM (Response a) -> AppM (Response a)
retry attempt go
  | attempt <= 0 = go
  | otherwise = go `catches` [Handler httpException, Handler jsonException]
 where
  httpException err@(HttpExceptionRequest _ (StatusCodeException response _)) =
    handle err response
  httpException (HttpExceptionRequest _ ConnectionTimeout) =
    retry (succ attempt) go
  httpException (HttpExceptionRequest _ ResponseTimeout) =
    retry (succ attempt) go
  httpException err = throwIO err
  jsonException err@(JSONParseException _ response _) = handle err response
  jsonException err@(JSONConversionException _ response _) =
    handle err response
  handle :: Exception e => e -> Response b -> AppM (Response a)
  handle err response = if getResponseStatusCode response == 429
    then do
      let seconds = getResponseDelay response
      logWarnN $ "Retrying after " <> T.pack (show seconds) <> " seconds"
      threadDelay $ seconds * 1000000
      retry (pred attempt) go
    else throwIO err

getResponseDelay :: Response a -> Int
getResponseDelay =
  fromMaybe 0 . readMaybe . T.unpack . decodeUtf8 . mconcat . getResponseHeader
    "Retry-After"
