{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- |
-- Module      : Chat.Flowdock.REST.Organisation
-- License     : BSD3
-- Maintainer  : Oleg Grenrus <oleg.grenrus@iki.fi>
module Chat.Flowdock.REST.Organisation (
  Organisation(..),
  OrganisationId,
  OrgLike(..),
  orgUsers,
  OrgUser(..),
  ouAdmin,
  ) where

import Prelude        ()
import Prelude.Compat

import Control.DeepSeq
import Control.Lens
import Data.Aeson.Compat
import Data.Binary.Orphans
import Data.Binary.Tagged
import Data.Hashable
import Data.Text
import Data.Typeable (Typeable)
import Data.Vector
import GHC.Generics as GHC
import Generics.SOP as SOP
import Text.PrettyPrint.ANSI.Leijen.AnsiPretty

import Chat.Flowdock.REST.Internal
import Chat.Flowdock.REST.User

data OrgUser = OrgUser
  { _ouId       :: !UserId
  , _ouNick     :: !Text
  , _ouName     :: !Text
  , _ouEmail    :: !Text
  , _ouAvatar   :: !Text
  , _ouWebsite  :: !(Maybe Text)
  , _ouAdmin    :: !Bool
  }
  deriving (Eq, Ord, Show, GHC.Generic, Typeable)

makeLenses ''OrgUser

instance NFData OrgUser
instance Hashable OrgUser
instance SOP.Generic OrgUser
instance SOP.HasDatatypeInfo OrgUser
instance Binary OrgUser
instance HasStructuralInfo OrgUser
instance HasSemanticVersion OrgUser

instance FromJSON OrgUser where
  parseJSON = withObject "User" $ \obj ->
    OrgUser <$> obj .: "id"
            <*> obj .: "nick"
            <*> obj .: "name"
            <*> obj .: "email"
            <*> obj .: "avatar"
            <*> obj .:? "website"
            <*> obj .: "admin"

instance AnsiPretty OrgUser

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
  , _orgUsers      :: !(Vector OrgUser)
  }
  deriving (Eq, Ord, Show, GHC.Generic, Typeable)

-- | Opaque Organisation identifier
type OrganisationId = Identifier Integer Organisation

makeLenses ''Organisation

instance NFData Organisation
--instance Hashable Organisation
instance SOP.Generic Organisation
instance SOP.HasDatatypeInfo Organisation
instance Binary Organisation
instance HasStructuralInfo Organisation
instance HasSemanticVersion Organisation

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

instance AnsiPretty Organisation

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
