{-# LANGUAGE ScopedTypeVariables #-}
module Chat.Flowdock.REST.Internal
  ( ApiUrl(..)
  , Identifier(..)
  , mkIdentifier
  , getIdentifier
  , ParamName(..)
  , mkParamName
  ) where

import Control.Applicative
import Control.DeepSeq
import Data.Aeson
import Data.Binary.Orphans
import Data.Binary.Tagged
import Data.Hashable
import Data.Proxy
import Text.PrettyPrint.ANSI.Leijen.AnsiPretty

-- | Opaque URL received from the API.
newtype ApiUrl res = ApiUrl String
  deriving (Eq, Ord, Show)

instance NFData (ApiUrl res) where
  rnf (ApiUrl url) = rnf url

instance Hashable (ApiUrl res) where
  hashWithSalt salt (ApiUrl url) = hashWithSalt salt url

instance Binary (ApiUrl res) where
  put (ApiUrl url) = put url
  get = ApiUrl <$> get

instance FromJSON (ApiUrl res) where
  parseJSON v = ApiUrl <$> parseJSON v

instance AnsiPretty (ApiUrl res) where
  ansiPretty (ApiUrl url) = ansiPretty url

newtype Identifier a res = Identifier a
  deriving (Eq, Ord, Show)

instance NFData a => NFData (Identifier a res) where
  rnf (Identifier x) = rnf x

instance Hashable a => Hashable (Identifier a res) where
  hashWithSalt salt (Identifier x) = hashWithSalt salt x

instance Binary a => Binary (Identifier a res) where
  put (Identifier x) = put x
  get = Identifier <$> get

instance HasStructuralInfo a => HasStructuralInfo (Identifier a res) where
  structuralInfo _ = NominalNewtype "Identifier" $ structuralInfo (Proxy :: Proxy a)

instance FromJSON a => FromJSON (Identifier a res) where
  parseJSON v = Identifier <$> parseJSON v

instance AnsiPretty a => AnsiPretty (Identifier a res) where
  ansiPretty (Identifier a) = ansiPretty a

mkIdentifier :: a -> Identifier a res
mkIdentifier = Identifier

getIdentifier :: Identifier a res -> a
getIdentifier (Identifier x) = x

-- | Semi-opaque parameterised name, used to construct requests
newtype ParamName res = ParamName String
  deriving (Eq, Ord, Show)

mkParamName :: String -> ParamName res
mkParamName = ParamName

getParamName :: ParamName res -> String
getParamName (ParamName param) = param

instance NFData (ParamName res) where
  rnf (ParamName param) = rnf param

instance Hashable (ParamName res) where
  hashWithSalt salt (ParamName param) = hashWithSalt salt param

instance Binary (ParamName res) where
  put (ParamName param) = put param
  get = ParamName <$> get

instance FromJSON (ParamName res) where
  parseJSON v = ParamName <$> parseJSON v

instance AnsiPretty (ParamName res) where
  ansiPretty (ParamName param) = ansiPretty param
