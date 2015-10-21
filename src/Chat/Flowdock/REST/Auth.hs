{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- |
-- Module      : Chat.Flowdock.REST.Auth
-- License     : BSD3
-- Maintainer  : Oleg Grenrus <oleg.grenrus@iki.fi>
module Chat.Flowdock.REST.Auth where

import Data.Typeable (Typeable)
import Data.ByteString.Char8 as BS8
import Network.HTTP.Client

-- | Authentication token.
--
-- See <https://www.flowdock.com/account/tokens> for your token.
newtype AuthToken = AuthToken { getAuthToken :: String }
  deriving (Show, Typeable)

-- | Add authorisation information to the request
authenticateRequest :: AuthToken -> Request -> Request
authenticateRequest (AuthToken token) = applyBasicAuth username "DUMMY"
  where username = BS8.pack token
