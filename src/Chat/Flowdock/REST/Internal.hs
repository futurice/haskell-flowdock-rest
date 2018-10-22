{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

import Prelude ()
import Prelude.Compat

import Control.DeepSeq
import Data.Aeson.Compat
import Data.Binary.Orphans
import Data.Binary.Tagged
import Data.Hashable
import Data.Proxy
import Data.String                             (fromString)
import Data.Text
import Data.Typeable                           (Typeable, typeRep)
import GHC.Generics
import Lucid                                   (ToHtml (..))
import Test.QuickCheck                         (Arbitrary (..))
import Text.PrettyPrint.ANSI.Leijen.AnsiPretty

import qualified Data.Csv                             as Csv
import qualified Data.Swagger                         as Swagger
import qualified Database.PostgreSQL.Simple.FromField as PQ
import qualified Database.PostgreSQL.Simple.ToField   as PQ
import qualified Web.HttpApiData                      as Web

-- | Opaque URL received from the API.
newtype ApiUrl res = ApiUrl String
  deriving (Eq, Ord, Show, Typeable)

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
  structuralInfo _ =
    NominalNewtype "ApiUrl" $ structuralInfo (Proxy :: Proxy String)

instance HasSemanticVersion (ApiUrl res)

-- | Semi-opaque identifier.
newtype Identifier a res = Identifier a
  deriving (Eq, Ord, Read, Show, Typeable)

instance NFData a => NFData (Identifier a res) where
  rnf (Identifier x) = rnf x

instance Hashable a => Hashable (Identifier a res) where
  hashWithSalt salt (Identifier x) = hashWithSalt salt x

instance Binary a => Binary (Identifier a res) where
  put (Identifier x) = put x
  get = Identifier <$> get

instance HasStructuralInfo a => HasStructuralInfo (Identifier a res) where
  structuralInfo _ =
    NominalNewtype "Identifier" $ structuralInfo (Proxy :: Proxy a)

instance HasSemanticVersion (Identifier a res)

instance FromJSON a => FromJSON (Identifier a res) where
  parseJSON v = Identifier <$> parseJSON v

instance ToJSON a => ToJSON (Identifier a res) where
    toJSON = toJSON . getIdentifier

instance AnsiPretty a => AnsiPretty (Identifier a res) where
  ansiPretty (Identifier a) = ansiPretty a

instance Show a => ToHtml (Identifier a res) where
    toHtmlRaw = toHtml
    toHtml = toHtml . show . getIdentifier

instance Csv.ToField a => Csv.ToField (Identifier a res) where
    toField = Csv.toField . getIdentifier

instance Arbitrary a =>  Arbitrary (Identifier a res) where
    arbitrary = Identifier <$> arbitrary

instance Swagger.ToParamSchema a => Swagger.ToParamSchema (Identifier a res) where
    toParamSchema _ = Swagger.toParamSchema (Proxy :: Proxy a )

instance (Swagger.ToSchema a, Typeable res) => Swagger.ToSchema (Identifier a res) where
    declareNamedSchema _ = do
        Swagger.NamedSchema _ s <- Swagger.declareNamedSchema (Proxy :: Proxy a)
        let name = "Identifier " ++ show (typeRep (Proxy :: Proxy res))
        pure $ Swagger.NamedSchema (Just $ fromString name) s

instance Web.ToHttpApiData a => Web.ToHttpApiData (Identifier a res) where
    toQueryParam = Web.toQueryParam . getIdentifier

instance Web.FromHttpApiData a => Web.FromHttpApiData (Identifier a res) where
    parseQueryParam = fmap Identifier . Web.parseQueryParam

instance PQ.ToField a => PQ.ToField (Identifier a res) where
    toField = PQ.toField .  getIdentifier

instance PQ.FromField a => PQ.FromField (Identifier a res) where
    fromField a b = Identifier <$> PQ.fromField a b

mkIdentifier :: a -> Identifier a res
mkIdentifier = Identifier

getIdentifier :: Identifier a res -> a
getIdentifier (Identifier x) = x

-- | Semi-opaque parameterised name, used to construct requests
newtype ParamName res = ParamName String
  deriving (Eq, Ord, Show, Typeable)

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
  structuralInfo _ =
    NominalNewtype "ParamName" $ structuralInfo (Proxy :: Proxy String)

instance HasSemanticVersion (ParamName res)

-- Tag

newtype Tag = Tag Text
  deriving (Eq, Ord, Show, Generic, Typeable)

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
