module Chat.Flowdock.REST.Common (
  FlowId,
  UserId,
  OrganisationId,
  ApiUrl,
  parseApiUrl,
  ParamName,
  mkParamName,
  ) where

import Control.Monad.Catch
import Network.HTTP.Client

import Chat.Flowdock.REST.Internal

parseApiUrl :: MonadThrow m => ApiUrl a -> m Request
parseApiUrl (ApiUrl url) = parseUrl url
