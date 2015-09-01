module Chat.Flowdock.REST.URLs (
  -- * Flows
  -- | See <https://www.flowdock.com/api/flows>
  flowsUrl,
  allFlowsUrl,
  flowGetUrl,
  -- * Users
  -- | See <https://www.flowdock.com/api/users>
  usersUrl,
  orgUsersUrl,
  flowUsersUrl,
  -- * Organisations
  -- | See <https://www.flowdock.com/api/organizations>
  organisationsUrl,
  organisationUrl,
  ) where

import Data.Monoid
import Data.List

import Chat.Flowdock.REST.Internal
import Chat.Flowdock.REST.User
import Chat.Flowdock.REST.Organisation
import Chat.Flowdock.REST.Flow

apiBaseUrl :: String
apiBaseUrl = "https://api.flowdock.com"

mkUrl :: [String] -> ApiUrl a
mkUrl parts = ApiUrl $ apiBaseUrl <> "/" <> intercalate "/" parts

flowsUrl :: ApiUrl [Flow]
flowsUrl = mkUrl ["flows"]

allFlowsUrl :: ApiUrl [Flow]
allFlowsUrl = mkUrl ["flows", "all"]

flowGetUrl :: ParamName Organisation -> ParamName Flow -> ApiUrl Flow
flowGetUrl (ParamName org) (ParamName flow) = mkUrl ["flows", org, flow]

usersUrl :: ApiUrl [User]
usersUrl = mkUrl ["users"]

flowUsersUrl :: ParamName Organisation -> ParamName Flow -> ApiUrl [User]
flowUsersUrl (ParamName org) (ParamName flow) = mkUrl ["flows", org, flow, "users"]

orgUsersUrl :: ParamName Organisation -> ApiUrl [User]
orgUsersUrl (ParamName org) = mkUrl ["organizations", org, "users"]

organisationsUrl :: ApiUrl [Organisation]
organisationsUrl = mkUrl ["organizations"]

organisationUrl :: ParamName Organisation -> ApiUrl Organisation
organisationUrl (ParamName org) = mkUrl ["organizations", org]
