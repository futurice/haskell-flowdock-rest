{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
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


data FlowOrg = FlowOrg
  { _foId :: !OrganisationId
  , _foName :: !Text
  , _foParamName :: !(ParamName Organisation)
  , _foUserLimit :: !Int
  , _foUserCount :: !Int
  , _foActive :: !Bool
  , _foUrl :: !(ApiUrl Organisation)
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

instance OrgLike FlowOrg where
  orgId = foId
  orgName = foName
  orgParamName = foParamName
  orgUserLimit = foUserLimit
  orgUserCount = foUserCount
  orgActive = foActive
  orgUrl = foUrl


data Flow = Flow
  { _flowId :: !FlowId
  , _flowName :: !Text
  , _flowParamName :: !(ParamName Flow)
  , _flowOrganisation :: !FlowOrg
  , _flowUrl :: !(ApiUrl Flow)
  , _flowWebUrl :: !Text
  , _flowAccessMode :: !FlowAccessMode
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

