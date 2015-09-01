{-# LANGUAGE OverloadedStrings #-}
module Chat.Flowdock.REST.Auth where

import Data.ByteString.Char8 as BS8
import Network.HTTP.Client

-- | Authentication token.
--
-- See <https://www.flowdock.com/account/tokens> for your token.
newtype AuthToken = AuthToken String
  deriving (Show)

-- | Add authorisation information to the request
authenticateRequest :: AuthToken -> Request -> Request
authenticateRequest (AuthToken token) = applyBasicAuth username "DUMMY"
  where username = BS8.pack token
