module Chat.Flowdock.REST.Internal where

import Control.Applicative
import Control.DeepSeq
import Data.Aeson
import Data.Hashable
import Text.PrettyPrint.ANSI.Leijen as PP hiding ((<>), (<$>))

-- | Opaque URL received from the API.
newtype ApiUrl res = ApiUrl String
  deriving (Eq, Ord, Show)

instance NFData (ApiUrl res) where
  rnf (ApiUrl url) = rnf url

instance Hashable (ApiUrl res) where
  hashWithSalt salt (ApiUrl url) = hashWithSalt salt url

instance FromJSON (ApiUrl res) where
  parseJSON v = ApiUrl <$> parseJSON v

instance Pretty (ApiUrl res) where
  pretty (ApiUrl url) = pretty url


-- | Opaque Organisation identifier
newtype FlowId = FlowId String
  deriving (Eq, Ord, Show)

instance NFData FlowId where
  rnf (FlowId fid) = rnf fid

instance Hashable FlowId where
  hashWithSalt salt (FlowId fid) = hashWithSalt salt fid

instance FromJSON FlowId where
  parseJSON v = FlowId <$> parseJSON v


-- | Opaque Message identifier
newtype MessageId = MessageId Integer
  deriving (Eq, Ord, Show)

instance NFData MessageId where
  rnf (MessageId mid) = rnf mid

instance Hashable MessageId where
  hashWithSalt salt (MessageId mid) = hashWithSalt salt mid

instance FromJSON MessageId where
  parseJSON v = MessageId <$> parseJSON v


-- | Opaque User identifier
newtype UserId = UserId Integer
  deriving (Eq, Ord, Show)

instance NFData UserId where
  rnf (UserId uid) = rnf uid

instance Hashable UserId where
  hashWithSalt salt (UserId uid) = hashWithSalt salt uid

instance FromJSON UserId where
  parseJSON v = UserId <$> parseJSON v

instance Pretty UserId where
  pretty (UserId uid) = pretty uid

-- | Opaque Organisation identifier
newtype OrganisationId = OrganisationId Integer
  deriving (Eq, Ord, Show)

instance NFData OrganisationId where
  rnf (OrganisationId oid) = rnf oid

instance Hashable OrganisationId where
  hashWithSalt salt (OrganisationId oid) = hashWithSalt salt oid

instance FromJSON OrganisationId where
  parseJSON v = OrganisationId <$> parseJSON v

instance Pretty OrganisationId where
  pretty (OrganisationId oid) = pretty oid

-- | Semi-opaque parameterised name, used to construct requests
newtype ParamName res = ParamName String
  deriving (Eq, Ord, Show)

mkParamName :: String -> ParamName res
mkParamName = ParamName

instance NFData (ParamName res) where
  rnf (ParamName param) = rnf param

instance Hashable (ParamName res) where
  hashWithSalt salt (ParamName param) = hashWithSalt salt param

instance FromJSON (ParamName res) where
  parseJSON v = ParamName <$> parseJSON v

instance Pretty (ParamName res) where
  pretty (ParamName param) = pretty param
