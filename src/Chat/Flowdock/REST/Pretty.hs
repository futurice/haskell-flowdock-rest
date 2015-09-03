{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
-- | This module should probably live somewhere else, contains helpers to define 'Pretty' instances.
module Chat.Flowdock.REST.Pretty (
  -- * Generic
  gpretty,
  gprettyWith,
  PrettyOpts(..),
  defPrettyOpts,
  prettyOpts,
  -- * Re-exports
  -- | 'Text.PrettyPrint.ANSI.Leijen'
   module PP,
  ) where

import Data.Char as C
import Data.List as L
import Data.Text as T
import Data.Monoid ((<>))
import Data.Time
import Generics.SOP
import Text.PrettyPrint.ANSI.Leijen as PP hiding ((<>), (<$>), semiBraces)

semiBraces :: [Doc] -> Doc
semiBraces = encloseSep (blue lbrace) (blue rbrace) (blue semi)

prettyField :: Pretty a => String -> a -> Doc
prettyField name value = magenta (text name) <+> blue equals <+> pretty value

prettyRecord :: String -> [Doc] -> Doc
prettyRecord name fields = hang 2 (green (text name) </> semiBraces fields)

instance Pretty Text where
  pretty = pretty . T.unpack

instance Pretty UTCTime where
  pretty = pretty . show

data PrettyOpts = PrettyOpts
  { poPrettyField :: FieldName -> Doc -> Doc
  , poPrettyRecord :: ConstructorName -> [Doc] -> Doc
  }

defPrettyOpts :: PrettyOpts
defPrettyOpts = PrettyOpts prettyField prettyRecord

-- | 'PrettyOpts' used in @flowdock-rest@
prettyOpts :: String -> PrettyOpts
prettyOpts prefix = defPrettyOpts { poPrettyField = poPrettyField defPrettyOpts . renamer }
  where renamer name| prefix `L.isPrefixOf` name  = dropTrailingPrime . lowerFirst .  Prelude.drop prefixLen $ name
                    | otherwise                   = name
        prefixLen = Prelude.length prefix
        lowerFirst (x:xs) = C.toLower x : xs
        lowerFirst xs     = xs
        dropTrailingPrime []      = []
        dropTrailingPrime ['\'']  = []
        dropTrailingPrime (x:xs)  = x : dropTrailingPrime xs


gprettyWith :: forall a. (Generic a, HasDatatypeInfo a, All2 Pretty (Code a)) => PrettyOpts -> a -> Doc
gprettyWith opts x = gprettyS opts (from x) (datatypeInfo (Proxy :: Proxy a))

gpretty :: forall a. (Generic a, HasDatatypeInfo a, All2 Pretty (Code a)) => a -> Doc
gpretty = gprettyWith defPrettyOpts

gprettyS :: (All2 Pretty xss) => PrettyOpts -> SOP I xss -> DatatypeInfo xss -> Doc
gprettyS _opts (SOP (Z (I x :* Nil))) (Newtype _ _ _)  = pretty x
gprettyS  opts (SOP (Z xs)) (ADT _ _ (ci :* Nil)) = poPrettyRecord opts (constructorName ci) (gprettyP opts xs (fieldInfo ci))
gprettyS _opts (SOP (Z _ )) _ = error "gprettyS: redundant Z case"
gprettyS  opts (SOP (S xss)) (ADT m d (_ :* cis)) = gprettyS opts (SOP xss) (ADT m d cis)
gprettyS _opts (SOP (S _)) _  = error "gprettyS: redundant S case"

gprettyP :: (All Pretty xs) => PrettyOpts -> NP I xs -> NP FieldInfo xs -> [Doc]
gprettyP _opts Nil Nil = []
gprettyP  opts (I x :* xs) (FieldInfo fieldName :* fis) = poPrettyField opts fieldName (pretty x) : gprettyP opts xs fis
gprettyP _opts _ _ = error "gprettyP: redundant case"

constructorName :: ConstructorInfo a -> ConstructorName
constructorName (Constructor name) = name
constructorName (Infix name _ _) = name
constructorName (Record name _) = name

fieldInfo :: ConstructorInfo xs -> NP FieldInfo xs
fieldInfo (Constructor _) = constructorFieldInfos 0 sing
fieldInfo (Infix _ _ _) = FieldInfo "_lhs" :* FieldInfo "_rhs" :* Nil
fieldInfo (Record _ fi) = fi

constructorFieldInfos :: forall (xs :: [*]). Int -> Sing xs -> NP FieldInfo xs
constructorFieldInfos _ SNil  = Nil
constructorFieldInfos n SCons = FieldInfo ("_" <> show n) :* constructorFieldInfos (n+1) sing
