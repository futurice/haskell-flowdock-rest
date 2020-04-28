-- |
-- Module      : Chat.Flowdock.REST.URLs
-- License     : BSD3
-- Maintainer  : Oleg Grenrus <oleg.grenrus@iki.fi>
module Chat.Flowdock.REST.URLs (
  -- * Flows
  -- | See <https://www.flowdock.com/api/flows>
  flowsUrl,
  allFlowsUrl,
  flowGetUrl,
  -- * Messages
  -- | See <https://www.flowdock.com/api/messages>
  messagesUrl,
  -- * Users
  -- | See <https://www.flowdock.com/api/users>
  usersUrl,
  organisationUsersUrl,
  flowUsersUrl,
  -- * Organisations
  -- | See <https://www.flowdock.com/api/organizations>
  organisationsUrl,
  organisationUrl,
  ) where

import Data.List (intercalate)

import Chat.Flowdock.REST.Internal
import Chat.Flowdock.REST.User
import Chat.Flowdock.REST.Organisation
import Chat.Flowdock.REST.Flow
import Chat.Flowdock.REST.Message

apiBaseUrl :: String
apiBaseUrl = "https://api.flowdock.com"

mkUrl :: [String] -> ApiUrl a
mkUrl parts = ApiUrl $ apiBaseUrl ++ "/" ++ intercalate "/" parts

flowsUrl :: ApiUrl [Flow]
flowsUrl = mkUrl ["flows"]

allFlowsUrl :: ApiUrl [Flow]
allFlowsUrl = mkUrl ["flows", "all"]

flowGetUrl :: ParamName Organisation -> ParamName Flow -> ApiUrl Flow
flowGetUrl (ParamName org) (ParamName flow) = mkUrl ["flows", org, flow]

messagesUrl :: ParamName Organisation -> ParamName Flow -> ApiUrl [Message]
messagesUrl (ParamName org) (ParamName flow) = mkUrl ["flows", org, flow, "messages"]

usersUrl :: ApiUrl [User]
usersUrl = mkUrl ["users"]

flowUsersUrl :: ParamName Organisation -> ParamName Flow -> ApiUrl [User]
flowUsersUrl (ParamName org) (ParamName flow) = mkUrl ["flows", org, flow, "users"]

organisationUsersUrl :: ParamName Organisation -> ApiUrl [User]
organisationUsersUrl (ParamName org) = mkUrl ["organizations", org, "users"]

organisationsUrl :: ApiUrl [Organisation]
organisationsUrl = mkUrl ["organizations"]

organisationUrl :: ParamName Organisation -> ApiUrl Organisation
organisationUrl (ParamName org) = mkUrl ["organizations", org]
