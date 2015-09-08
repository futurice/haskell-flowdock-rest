{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
module Chat.Flowdock.REST.Request (
  -- * Request creation
  parseApiUrl,
  -- * Messages
  -- | See <https://www.flowdock.com/api/messages>
  messagesRequest,
  -- ** Options
  MessageOptions,
  defMessageOptions,
  msgOptEvents,
  msgOptLimit,
  msgOptUntilId,
  msgOptSinceId,
  msgOptSorting,
  Sorting(..),
  sortingToString,
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Data.ByteString
import Data.Maybe
import Data.List as L
import Data.List.NonEmpty
import Data.String
import Data.Tagged
import Network.HTTP.Client

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
parseApiUrl (ApiUrl url) = Tagged `liftM` parseUrl url

data Sorting = Descending | Ascending
  deriving (Eq, Ord, Read, Show, Enum, Bounded)

sortingToString :: Sorting -> String
sortingToString Descending = "desc"
sortingToString Ascending = "asc"

data MessageOptions = MessageOptions
  { _msgOptEvents :: [MessageEvent]
  , _msgOptLimit :: Maybe Int
  , _msgOptUntilId :: Maybe MessageId
  , _msgOptSinceId :: Maybe MessageId
  , _msgOptSorting :: Sorting
  }
  deriving (Eq, Ord, Show)

defMessageOptions :: MessageOptions
defMessageOptions = MessageOptions [] Nothing Nothing Nothing Descending

makeLenses ''MessageOptions

messagesRequest :: MonadThrow m => ParamName Organisation -> ParamName Flow -> MessageOptions -> m (Tagged [Message] Request)
messagesRequest org flow MessageOptions {..} = do
  req <- parseApiUrl (messagesUrl org flow)
  return $ setQueryString queryString <$> req
  where queryString = catMaybes [ (\es -> ("event",    Just $ fromString $ L.intercalate "," $ toList $ fmap messageEventToString es)) <$> nonEmpty _msgOptEvents
                                , (\l  -> ("limit",    Just $ fromString $ show l))                 <$> _msgOptLimit
                                , (\u  -> ("until_id", Just $ fromString $ show $ getIdentifier u)) <$> _msgOptUntilId
                                , (\s  -> ("since_id", Just $ fromString $ show $ getIdentifier s)) <$> _msgOptSinceId
                                , Just ("sort", Just $ fromString $ sortingToString _msgOptSorting)
                                ]
