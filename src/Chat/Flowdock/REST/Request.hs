{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
-- |
-- Module      : Chat.Flowdock.REST.Request
-- License     : BSD3
-- Maintainer  : Oleg Grenrus <oleg.grenrus@iki.fi>
module Chat.Flowdock.REST.Request (
  -- * Request creation
  parseApiUrl,
  -- * Flows
  -- | See <https://www.flowdock.com/api/flows>
  flowsRequest,
  allFlowsRequest,
  flowRequest,
  -- * Messages
  -- | See <https://www.flowdock.com/api/messages>
  messagesRequest,
  -- ** Options
  MessageOptions,
  defaultMessageOptions,
  msgOptEvents,
  msgOptLimit,
  msgOptUntilId,
  msgOptSinceId,
  msgOptTags,
  msgOptTagMode,
  msgOptSorting,
  Sorting(..),
  sortingToString,
  TagMode(..),
  tagModeToString,
  -- * Users
  -- | See <https://www.flowdock.com/api/users>
  usersRequest,
  organisationUsersRequest,
  flowUsersRequest,
  -- * Organisations
  -- | See <https://www.flowdock.com/api/organizations>
  organisationsRequest,
  organisationRequest,
  -- * Deprecated
  defMessageOptions,
  ) where

import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Data.Maybe (mapMaybe)
import Data.List.NonEmpty
import Data.String
import Data.Tagged
import Network.HTTP.Client

import qualified Data.List as L
import qualified Data.Text as T

import Chat.Flowdock.REST.Internal
import Chat.Flowdock.REST.User
import Chat.Flowdock.REST.Organisation
import Chat.Flowdock.REST.Flow
import Chat.Flowdock.REST.Message

import Chat.Flowdock.REST.URLs

-- | Convert a 'ApiUrl' into a 'Request'.
--
-- See 'Network.HTTP.Client.parseUrl'
--
-- Since this function uses 'MonadThrow', the return monad can be anything that is an instance of 'MonadThrow', such as 'IO' or 'Maybe'.
parseApiUrl :: MonadThrow m => ApiUrl a -> m (Tagged a Request)
parseApiUrl (ApiUrl url) = Tagged `liftM` parseUrlThrow url

data Sorting = Descending | Ascending
  deriving (Eq, Ord, Read, Show, Enum, Bounded)

sortingToString :: Sorting -> String
sortingToString Descending = "desc"
sortingToString Ascending = "asc"

data TagMode
  = TagModeAnd
  | TagModeOr
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

tagModeToString :: TagMode -> String
tagModeToString TagModeAnd = "and"
tagModeToString TagModeOr  = "or"

-- | See <https://www.flowdock.com/api/messages>
data MessageOptions = MessageOptions
  { _msgOptEvents  :: [MessageEvent]
  , _msgOptLimit   :: Maybe Int
  , _msgOptUntilId :: Maybe MessageId
  , _msgOptSinceId :: Maybe MessageId
  , _msgOptTags    :: [Tag]
  , _msgOptTagMode :: TagMode
  , _msgOptSorting :: Sorting
  }
  deriving (Eq, Ord, Show)


defaultMessageOptions :: MessageOptions
defaultMessageOptions = MessageOptions [] Nothing Nothing Nothing [] TagModeAnd Descending

defMessageOptions :: MessageOptions
defMessageOptions = defaultMessageOptions
{-# DEPRECATED defMessageOptions "use defaultMessageOptions" #-}

makeLenses ''MessageOptions

messagesRequest :: MonadThrow m => ParamName Organisation -> ParamName Flow -> MessageOptions -> m (Tagged [Message] Request)
messagesRequest org flow MessageOptions {..} = do
  req <- parseApiUrl (messagesUrl org flow)
  return $ setQueryString queryString <$> req
  where
    queryString = mapMaybe (fmap mk)
      [ (\es -> ("event",    L.intercalate "," $ toList $ fmap messageEventToString es)) <$> nonEmpty _msgOptEvents
      , (\l  -> ("limit",    show l))                 <$> _msgOptLimit
      , (\u  -> ("until_id", show $ getIdentifier u)) <$> _msgOptUntilId
      , (\s  -> ("since_id", show $ getIdentifier s)) <$> _msgOptSinceId
      , Just ("sort", sortingToString _msgOptSorting)
      , ("tag_mode", tagModeToString _msgOptTagMode) <$ tagsParam
      , (\ts -> ("tags", ts)) <$> tagsParam
      ]

    tagsParam :: Maybe String
    tagsParam
      | null _msgOptTags = Nothing
      | otherwise = Just $ T.unpack $ T.intercalate "," $ getTag <$> _msgOptTags

    mk (k, v) = (k, Just $ fromString v)

-- Flows

flowsRequest :: MonadThrow m => m (Tagged [Flow] Request)
flowsRequest = parseApiUrl flowsUrl

allFlowsRequest :: MonadThrow m => m (Tagged [Flow] Request)
allFlowsRequest = parseApiUrl allFlowsUrl

flowRequest :: MonadThrow m => ParamName Organisation -> ParamName Flow -> m (Tagged Flow Request)
flowRequest org flow = parseApiUrl $ flowGetUrl org flow

-- Users

usersRequest :: MonadThrow m => m (Tagged [User] Request)
usersRequest = parseApiUrl usersUrl

flowUsersRequest :: MonadThrow m => ParamName Organisation -> ParamName Flow -> m (Tagged [User] Request)
flowUsersRequest org flow = parseApiUrl $ flowUsersUrl org flow

organisationUsersRequest :: MonadThrow m => ParamName Organisation -> m (Tagged [User] Request)
organisationUsersRequest org = parseApiUrl $ organisationUsersUrl org

-- Organisations

organisationsRequest :: MonadThrow m => m (Tagged [Organisation] Request)
organisationsRequest = parseApiUrl organisationsUrl

organisationRequest :: MonadThrow m => ParamName Organisation -> m (Tagged Organisation Request)
organisationRequest org = parseApiUrl $ organisationUrl org
