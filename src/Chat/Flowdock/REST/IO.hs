-- |
-- Module      : Chat.Flowdock.REST.IO
-- License     : BSD3
-- Maintainer  : Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- Higher level IO functionality
{-# LANGUAGE DeriveDataTypeable #-}
module Chat.Flowdock.REST.IO (
  -- * Flows
  -- | See <https://www.flowdock.com/api/flows>
  flows,
  allFlows,
  flow,
  -- * Messages
  -- | See <https://www.flowdock.com/api/messages>
  messages,
  -- ** Options
  MessageOptions,
  defMessageOptions,
  msgOptEvents,
  msgOptLimit,
  msgOptUntilId,
  msgOptSinceId,
  msgOptSorting,
  Sorting(..),
  -- * Users
  -- | See <https://www.flowdock.com/api/users>
  users,
  organisationUsers,
  flowUsers,
  -- * Organisations
  -- | See <https://www.flowdock.com/api/organizations>
  organisations,
  organisation,
  ) where

import           Control.Monad.Catch (MonadThrow(..))
import           Control.Monad.IO.Class
import           Data.Aeson.Compat (FromJSON(..), decode')
import           Data.Tagged (Tagged, untag)
import           Network.HTTP.Client (httpLbs, responseBody, Manager, Request)

import           Chat.Flowdock.REST

performRequest :: (MonadIO m, MonadThrow m, FromJSON a) => Manager -> AuthToken -> Tagged a Request -> m a
performRequest mgr token req = do
  let req' = authenticateRequest token (untag req)
  res <- liftIO $ httpLbs req' mgr
  decode' (responseBody res)

messages :: (MonadIO m, MonadThrow m) => Manager -> AuthToken -> ParamName Organisation -> ParamName Flow -> MessageOptions -> m [Message]
messages mgr token org fl opts = performRequest mgr token =<< messagesRequest org fl opts


-- Flows

flows :: (MonadIO m, MonadThrow m) => Manager -> AuthToken -> m [Flow]
flows mgr token = performRequest mgr token =<< flowsRequest

allFlows :: (MonadIO m, MonadThrow m) => Manager -> AuthToken -> m [Flow]
allFlows mgr token = performRequest mgr token =<< allFlowsRequest

flow ::(MonadIO m, MonadThrow m) => Manager -> AuthToken -> ParamName Organisation -> ParamName Flow -> m Flow
flow mgr token org fl = performRequest mgr token =<< flowRequest org fl

-- Users

users :: (MonadIO m, MonadThrow m) => Manager -> AuthToken -> m [User]
users mgr token = performRequest mgr token =<< usersRequest

flowUsers :: (MonadIO m, MonadThrow m) => Manager -> AuthToken -> ParamName Organisation -> ParamName Flow -> m [User]
flowUsers mgr token org fl = performRequest mgr token =<< flowUsersRequest org fl

organisationUsers :: (MonadIO m, MonadThrow m) => Manager -> AuthToken -> ParamName Organisation -> m [User]
organisationUsers mgr token org = performRequest mgr token =<< organisationUsersRequest org

-- Organisations

organisations :: (MonadIO m, MonadThrow m) => Manager -> AuthToken -> m [Organisation]
organisations mgr token = performRequest mgr token =<< organisationsRequest

organisation :: (MonadIO m, MonadThrow m) => Manager -> AuthToken -> ParamName Organisation -> m Organisation
organisation mgr token org = performRequest mgr token =<< organisationRequest org
