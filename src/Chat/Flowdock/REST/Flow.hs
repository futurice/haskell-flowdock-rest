{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
module Chat.Flowdock.REST.Flow where

import Control.Applicative
import Control.DeepSeq
import Control.Lens
import Data.Aeson
import Data.Hashable
import Data.Monoid
import Data.Text as T
import GHC.Generics

import Chat.Flowdock.REST.Internal
import Chat.Flowdock.REST.Organisation
import Chat.Flowdock.REST.Pretty

data FlowAccessMode = FAMInvintation
                    | FAMLink
                    | FAMOrganisation
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

instance NFData FlowAccessMode
instance Hashable FlowAccessMode

instance FromJSON FlowAccessMode where
  parseJSON = withText "Flow access mode" p
    where p "invitation"    = pure FAMInvintation
          p "link"          = pure FAMLink
          p "organization"  = pure FAMOrganisation
          p x               = fail $ "Invalid flow access mode: " <> T.unpack x


instance Pretty FlowAccessMode where
  pretty FAMInvintation   = text "invintation"
  pretty FAMLink          = text "link"
  pretty FAMOrganisation  = text "organisation"

data FlowOrg = FlowOrg
  { _foId        :: !OrganisationId
  , _foName      :: !Text
  , _foParamName :: !(ParamName Organisation)
  , _foUserLimit :: !Int
  , _foUserCount :: !Int
  , _foActive    :: !Bool
  , _foUrl       :: !(ApiUrl Organisation)
  }
  deriving (Eq, Ord, Show, Generic)

makeLenses ''FlowOrg

instance NFData FlowOrg
instance Hashable FlowOrg

instance FromJSON FlowOrg where
  parseJSON = withObject "Flow Org" $ \obj ->
    FlowOrg <$> obj .: "id"
            <*> obj .: "name"
            <*> obj .: "parameterized_name"
            <*> obj .: "user_limit"
            <*> obj .: "user_count"
            <*> obj .: "active"
            <*> obj .: "url"

instance Pretty FlowOrg where
  pretty FlowOrg {..} = prettyRecord "FlowOrg"
    [ prettyField "id" _foId
    , prettyField "param_name" _foParamName
    , prettyField "name" $ prettyText _foName
    , prettyField "user_limit" _foUserLimit
    , prettyField "user_count" _foUserCount
    , prettyField "active" _foActive
    , prettyField "url" _foUrl
    ]

instance OrgLike FlowOrg where
  orgId = foId
  orgName = foName
  orgParamName = foParamName
  orgUserLimit = foUserLimit
  orgUserCount = foUserCount
  orgActive = foActive
  orgUrl = foUrl


data Flow = Flow
  { _flowId           :: !FlowId
  , _flowName         :: !Text
  , _flowParamName    :: !(ParamName Flow)
  , _flowOrganisation :: !FlowOrg
  , _flowUrl          :: !(ApiUrl Flow)
  , _flowWebUrl       :: !Text
  , _flowAccessMode   :: !FlowAccessMode
  }
  deriving (Eq, Ord, Show, Generic)

makeLenses ''Flow

instance NFData Flow
instance Hashable Flow

instance FromJSON Flow where
  parseJSON = withObject "Flow" $ \obj ->
    Flow <$> obj .: "id"
         <*> obj .: "name"
         <*> obj .: "parameterized_name"
         <*> obj .: "organization"
         <*> obj .: "url"
         <*> obj .: "web_url"
         <*> obj .: "access_mode"

instance Pretty Flow where
  pretty Flow {..} = prettyRecord "Flow"
    [ prettyField "id" _flowId
    , prettyField "param_name" _flowParamName
    , prettyField "name" $ prettyText _flowName
    , prettyField "org" _flowOrganisation
    , prettyField "url" _flowUrl
    , prettyField "accessMode" _flowAccessMode
    ]
