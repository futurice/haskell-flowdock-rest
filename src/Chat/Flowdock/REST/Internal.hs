{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
-- |
-- Module      : Chat.Flowdock.REST.Internal
-- License     : BSD3
-- Maintainer  : Oleg Grenrus <oleg.grenrus@iki.fi>
module Chat.Flowdock.REST.Internal
  ( ApiUrl(..)
  , Identifier(..)
  , mkIdentifier
  , getIdentifier
  , ParamName(..)
  , mkParamName
  , getParamName
  , Tag(..)
  , getTag
  ) where

import Control.Applicative
import Control.DeepSeq
import Data.Aeson
import Data.Binary.Orphans
import Data.Binary.Tagged
import Data.Hashable
import Data.Proxy
import Data.Text
import GHC.Generics
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

instance HasStructuralInfo (ApiUrl res) where
  structuralInfo _ = NominalNewtype "ApiUrl" $ structuralInfo (Proxy :: Proxy String)

instance HasSemanticVersion (ApiUrl res)

-- | Semi-opaque identifier.
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

instance HasSemanticVersion (Identifier a res)

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

instance HasStructuralInfo (ParamName res) where
  structuralInfo _ = NominalNewtype "ParamName" $ structuralInfo (Proxy :: Proxy String)

instance HasSemanticVersion (ParamName res)

-- Tag

newtype Tag = Tag Text
  deriving (Eq, Ord, Show, Generic)

getTag :: Tag -> Text
getTag (Tag tag) = tag

instance NFData Tag
instance Hashable Tag
instance Binary Tag
instance FromJSON Tag where
  parseJSON v = Tag <$> parseJSON v
instance AnsiPretty Tag
instance HasStructuralInfo Tag
instance HasSemanticVersion Tag
