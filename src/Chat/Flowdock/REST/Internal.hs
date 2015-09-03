module Chat.Flowdock.REST.Internal
  ( ApiUrl(..)
  , Identifier(..)
  , mkIdentifier
  , getIdentifier
  , ParamName(..)
  , mkParamName
  , FlowId, MessageId, UserId, OrganisationId
  ) where

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

newtype Identifier a res = Identifier a
  deriving (Eq, Ord, Show)

instance NFData a => NFData (Identifier a res) where
  rnf (Identifier x) = rnf x

instance Hashable a => Hashable (Identifier a res) where
  hashWithSalt salt (Identifier x) = hashWithSalt salt x

instance FromJSON a => FromJSON (Identifier a res) where
  parseJSON v = Identifier <$> parseJSON v

instance Pretty a => Pretty (Identifier a res) where
  pretty (Identifier a) = pretty a

mkIdentifier :: a -> Identifier a res
mkIdentifier = Identifier

getIdentifier :: Identifier a res -> a
getIdentifier (Identifier x) = x

-- Non exported tags
data Flow
data Message
data User
data Organisation

-- | Opaque Organisation identifier
type FlowId = Identifier String Flow

-- | Opaque Message identifiert
type MessageId = Identifier Integer Message

-- | Opaque User identifier
type UserId = Identifier Integer User

-- | Opaque Organisation identifier
type OrganisationId = Identifier Integer Organisation

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
