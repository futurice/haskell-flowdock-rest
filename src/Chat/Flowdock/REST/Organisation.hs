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
import Data.Text as T
import GHC.Generics
import Text.PrettyPrint.ANSI.Leijen as PP hiding ((<>), (<$>))

import Chat.Flowdock.REST.Internal
import Chat.Flowdock.REST.User

data OrgUser = OrgUser
  { _ouId :: !UserId
  , _ouNick :: !Text
  , _ouName :: !Text
  , _ouEmail :: !Text
  , _ouAvatar :: !Text
  , _ouWebsite :: !(Maybe Text)
  , _ouAdmin :: !Bool
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

prettyField :: Pretty a => String -> a -> Doc
prettyField name value = text name <+> equals <+> pretty value

instance Pretty OrgUser where
  pretty OrgUser {..} = text "OrgUser" <+> semiBraces
    [ prettyField "id" _ouId
    , prettyField "nick" (T.unpack _ouNick)
    , prettyField "name" (T.unpack _ouName)  
    , prettyField "email" (T.unpack _ouEmail)
    , prettyField "avatar" (T.unpack _ouAvatar)
    , prettyField "website" (T.unpack <$> _ouWebsite)
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
  { _orgId' :: !OrganisationId
  , _orgParamName' :: !(ParamName Organisation)
  , _orgName' :: !Text
  , _orgUserLimit' :: !Int
  , _orgUserCount' :: !Int
  , _orgActive' :: !Bool
  , _orgUrl' :: !(ApiUrl Organisation)
  , _orgUsers :: ![OrgUser]
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
  pretty Organisation {..} = text "Organisation" <+> semiBraces
    [ prettyField "name" _orgId'
    , prettyField "param_name" _orgParamName'
    , prettyField "name" (T.unpack _orgName')  
    , prettyField "user_limit" _orgUserLimit'
    , prettyField "user_count" _orgUserCount'
    , prettyField "active" _orgActive'
    , prettyField "url" _orgUrl'
    , prettyField "users" _orgUsers
    ]

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
