{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
module Chat.Flowdock.REST.Organisation (
  Organisation(..),
  OrgLike(..),
  orgUsers,
  OrgUser(..),
  ouAdmin,
  ) where

import Control.Applicative
import Control.DeepSeq
import Control.Lens
import Data.Aeson
import Data.Hashable
import Data.Text
import GHC.Generics

import Chat.Flowdock.REST.Internal
import Chat.Flowdock.REST.User
import Chat.Flowdock.REST.Pretty

data OrgUser = OrgUser
  { _ouId       :: !UserId
  , _ouNick     :: !Text
  , _ouName     :: !Text
  , _ouEmail    :: !Text
  , _ouAvatar   :: !Text
  , _ouWebsite  :: !(Maybe Text)
  , _ouAdmin    :: !Bool
  }
  deriving (Eq, Ord, Show, Generic)

makeLenses ''OrgUser

instance NFData OrgUser
instance Hashable OrgUser

instance FromJSON OrgUser where
  parseJSON = withObject "User" $ \obj ->
    OrgUser <$> obj .: "id"
            <*> obj .: "nick"
            <*> obj .: "name"
            <*> obj .: "email"
            <*> obj .: "avatar"
            <*> obj .:? "website"
            <*> obj .: "admin"

instance Pretty OrgUser where
  pretty OrgUser {..} = prettyRecord "OrgUser"
    [ prettyField "id" _ouId
    , prettyField "nick" $ prettyText _ouNick
    , prettyField "name" $ prettyText _ouName
    , prettyField "email" $ prettyText _ouEmail
    , prettyField "avatar" $ prettyText _ouAvatar
    , prettyField "website" $ prettyFunctorText _ouWebsite
    , prettyField "admin" _ouAdmin
    ]

instance UserLike OrgUser where
  userId = ouId
  userNick = ouNick
  userName = ouName
  userEmail = ouEmail
  userAvatar = ouAvatar
  userWebsite = ouWebsite


data Organisation = Organisation
  { _orgId'        :: !OrganisationId
  , _orgParamName' :: !(ParamName Organisation)
  , _orgName'      :: !Text
  , _orgUserLimit' :: !Int
  , _orgUserCount' :: !Int
  , _orgActive'    :: !Bool
  , _orgUrl'       :: !(ApiUrl Organisation)
  , _orgUsers      :: ![OrgUser]
  }
  deriving (Eq, Ord, Show, Generic)

makeLenses ''Organisation

instance NFData Organisation
instance Hashable Organisation

instance FromJSON Organisation where
  parseJSON = withObject "Organisation" $ \obj ->
    Organisation <$> obj .: "id"
                 <*> obj .: "parameterized_name"
                 <*> obj .: "name"
                 <*> obj .: "user_limit"
                 <*> obj .: "user_count"
                 <*> obj .: "active"
                 <*> obj .: "url"
                 <*> obj .: "users"

instance Pretty Organisation where
  pretty Organisation {..} = prettyRecord "Organisation"
    [ prettyField "id" _orgId'
    , prettyField "param_name" _orgParamName'
    , prettyField "name" $ prettyText _orgName'
    , prettyField "user_limit" _orgUserLimit'
    , prettyField "user_count" _orgUserCount'
    , prettyField "active" _orgActive'
    , prettyField "url" _orgUrl'
    , prettyField "users" _orgUsers
    ]

-- | 'Organisation' like structures
class OrgLike o where
  orgId :: Lens' o OrganisationId
  orgName :: Lens' o Text
  orgParamName :: Lens' o (ParamName Organisation)
  orgUserLimit :: Lens' o Int
  orgUserCount :: Lens' o Int
  orgActive :: Lens' o Bool
  orgUrl :: Lens' o (ApiUrl Organisation)

instance OrgLike Organisation where
  orgId = orgId'
  orgName = orgName'
  orgParamName = orgParamName'
  orgUserLimit = orgUserLimit'
  orgUserCount = orgUserCount'
  orgActive = orgActive'
  orgUrl = orgUrl'
