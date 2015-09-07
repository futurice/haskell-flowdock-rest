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
  msgOptEvent,
  msgOptLimit,
  msgOptUntilId,
  msgOptSinceId,
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Data.ByteString
import Data.Maybe
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

data MessageOptions = MessageOptions
  { _msgOptEvent :: Maybe MessageEvent
  , _msgOptLimit :: Maybe Int
  , _msgOptUntilId :: Maybe MessageId
  , _msgOptSinceId :: Maybe MessageId
  }
  deriving (Eq, Ord, Show)

defMessageOptions :: MessageOptions
defMessageOptions = MessageOptions Nothing Nothing Nothing Nothing

makeLenses ''MessageOptions

messagesRequest :: MonadThrow m => ParamName Organisation -> ParamName Flow -> MessageOptions -> m (Tagged [Message] Request)
messagesRequest org flow MessageOptions {..} = do
  req <- parseApiUrl (messagesUrl org flow)
  return $ setQueryString queryString <$> req
  where queryString = catMaybes [ (\e -> ("event",    Just $ fromString $ messageEventToString e)) <$> _msgOptEvent
                                , (\l -> ("limit",    Just $ fromString $ show l))                 <$> _msgOptLimit
                                , (\u -> ("until_id", Just $ fromString $ show $ getIdentifier u)) <$> _msgOptUntilId
                                , (\s -> ("since_id", Just $ fromString $ show $ getIdentifier s)) <$> _msgOptSinceId
                                ]
