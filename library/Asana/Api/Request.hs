module Asana.Api.Request
  ( Single(..)
  , Page(..)
  , NextPage(..)
  , getAll
  , getAllParams
  , getSingle
  , put
  , post
  , maxRequests
  ) where

import RIO

import Asana.App (AppM, appApiAccessKey)
import Control.Monad (when)
import Data.Aeson (FromJSON, ToJSON, Value, genericParseJSON, parseJSON)
import Data.Aeson.Casing (aesonPrefix, snakeCase)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Network.HTTP.Simple
  ( JSONException(JSONConversionException, JSONParseException)
  , Request
  , Response
  , addRequestHeader
  , getResponseBody
  , getResponseHeader
  , getResponseStatusCode
  , httpJSON
  , parseRequest
  , setRequestBodyJSON
  , setRequestMethod
  )
import Prelude (pred)
import RIO.Text (Text)
import qualified RIO.Text as T
import Text.Read (readMaybe)

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
getAll :: FromJSON a => String -> AppM ext [a]
getAll path = getAllParams path []

getAllParams :: FromJSON a => String -> [(String, String)] -> AppM ext [a]
getAllParams path params = go Nothing
 where
  go mOffset = do
    Page d mNextPage <- get path params 50 mOffset

    maybe (pure d) (fmap (d ++) . go . Just . T.unpack . npOffset) mNextPage

-- | Get a single resource
getSingle :: FromJSON a => String -> AppM ext a
getSingle path = sData <$> get path [] 1 Nothing

get
  :: FromJSON a
  => String
  -> [(String, String)]
  -> Int
  -> Maybe String
  -> AppM ext a
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
  response <- retry 50 $ httpJSON (addAuthorization auth request)
  when (300 <= getResponseStatusCode response)
    . logWarn
    $ "GET failed "
    <> display (getResponseStatusCode response)
  pure $ getResponseBody response

put :: ToJSON a => String -> a -> AppM ext Value
put = httpAction "PUT"

post :: ToJSON a => String -> a -> AppM ext Value
post = httpAction "POST"

httpAction :: ToJSON a => ByteString -> String -> a -> AppM ext Value
httpAction verb path payload = do
  auth <- asks appApiAccessKey
  request <- parseRequest $ "https://app.asana.com/api/1.0" <> path

  response <- retry 10 $ httpJSON
    (setRequestMethod verb . setRequestBodyJSON payload $ addAuthorization
      auth
      request
    )
  when (300 <= getResponseStatusCode response)
    . logWarn
    $ displayBytesUtf8 verb
    <> " failed "
    <> display (getResponseStatusCode response)
    <> " "
    <> displayShow (getResponseBody @Value response)

  pure $ getResponseBody response

addAuthorization :: Text -> Request -> Request
addAuthorization auth =
  addRequestHeader "Authorization" $ "Bearer " <> T.encodeUtf8 auth

retry :: forall a ext . Int -> AppM ext (Response a) -> AppM ext (Response a)
retry attempt go
  | attempt <= 0 = go
  | otherwise = handler =<< go `catch` handleParseError
 where
  handleParseError :: JSONException -> AppM ext (Response a)
  handleParseError e = case e of
    JSONParseException _ rsp _ -> orThrow e rsp
    JSONConversionException _ rsp _ -> orThrow e rsp

  orThrow :: Exception e => e -> Response b -> AppM ext (Response a)
  orThrow e response
    | getResponseStatusCode response == 429 = do
      let seconds = getResponseDelay response
      logWarn $ "Retrying after " <> display seconds <> " seconds"
      threadDelay $ seconds * 1000000
      retry (pred attempt) go
    | otherwise = liftIO $ throwIO e

  handler :: Response a -> AppM ext (Response a)
  handler response
    | getResponseStatusCode response == 429 = do
      let seconds = getResponseDelay response
      logWarn $ "Retrying after " <> display seconds <> " seconds"
      threadDelay $ seconds * 100000
      retry (pred attempt) go
    | otherwise = pure response

getResponseDelay :: Response a -> Int
getResponseDelay =
  fromMaybe 0
    . readMaybe
    . T.unpack
    . T.decodeUtf8With T.lenientDecode
    . mconcat
    . getResponseHeader "Retry-After"
