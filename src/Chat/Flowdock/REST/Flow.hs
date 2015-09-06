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
import GHC.Generics as GHC
import Generics.SOP as SOP

import Chat.Flowdock.REST.Internal
import Chat.Flowdock.REST.Organisation
import Chat.Flowdock.REST.Pretty

data FlowAccessMode = FAMInvintation
                    | FAMLink
                    | FAMOrganisation
  deriving (Eq, Ord, Show, Enum, Bounded, GHC.Generic)

instance NFData FlowAccessMode
instance Hashable FlowAccessMode

instance FromJSON FlowAccessMode where
  parseJSON = withText "Flow access mode" p
    where p "invitation"    = pure FAMInvintation
          p "link"          = pure FAMLink
          p "organization"  = pure FAMOrganisation
          p x               = fail $ "Invalid flow access mode: " <> T.unpack x


instance AnsiPretty FlowAccessMode where
  ansiPretty FAMInvintation   = text "invintation"
  ansiPretty FAMLink          = text "link"
  ansiPretty FAMOrganisation  = text "organisation"

data FlowOrg = FlowOrg
  { _foId        :: !OrganisationId
  , _foName      :: !Text
  , _foParamName :: !(ParamName Organisation)
  , _foUserLimit :: !Int
  , _foUserCount :: !Int
  , _foActive    :: !Bool
  , _foUrl       :: !(ApiUrl Organisation)
  }
  deriving (Eq, Ord, Show, GHC.Generic)

makeLenses ''FlowOrg

instance NFData FlowOrg
instance Hashable FlowOrg
instance SOP.Generic FlowOrg
instance SOP.HasDatatypeInfo FlowOrg

instance FromJSON FlowOrg where
  parseJSON = withObject "Flow Org" $ \obj ->
    FlowOrg <$> obj .: "id"
            <*> obj .: "name"
            <*> obj .: "parameterized_name"
            <*> obj .: "user_limit"
            <*> obj .: "user_count"
            <*> obj .: "active"
            <*> obj .: "url"

instance AnsiPretty FlowOrg where
  ansiPretty = gprettyWith (prettyOpts "_fo")

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
  deriving (Eq, Ord, Show, GHC.Generic)

makeLenses ''Flow

instance NFData Flow
instance Hashable Flow
instance SOP.Generic Flow
instance SOP.HasDatatypeInfo Flow

instance FromJSON Flow where
  parseJSON = withObject "Flow" $ \obj ->
    Flow <$> obj .: "id"
         <*> obj .: "name"
         <*> obj .: "parameterized_name"
         <*> obj .: "organization"
         <*> obj .: "url"
         <*> obj .: "web_url"
         <*> obj .: "access_mode"

instance AnsiPretty Flow where
  ansiPretty = gprettyWith (prettyOpts "_flow")
