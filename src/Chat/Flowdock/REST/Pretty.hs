{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
-- | This module should probably live somewhere else (in own package).
module Chat.Flowdock.REST.Pretty (
  -- * Class
  AnsiPretty(..),
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
import Text.PrettyPrint.ANSI.Leijen as PP hiding ((<>), (<$>), semiBraces, Pretty)

class AnsiPretty a where
  ansiPretty :: a -> Doc

  ansiPrettyList :: [a] -> Doc
  ansiPrettyList = encloseSep (dullgreen lbracket) (dullgreen rbracket) (dullgreen colon) . fmap ansiPretty

semiBraces :: [Doc] -> Doc
semiBraces = encloseSep (dullblue lbrace) (dullblue rbrace) (dullblue semi)

prettyField :: AnsiPretty a => String -> a -> Doc
prettyField name value = black (text name) <+> blue equals <+> ansiPretty value

prettyRecord :: String -> [Doc] -> Doc
prettyRecord name fields = hang 2 (cyan (text name) </> semiBraces fields)

instance AnsiPretty Integer where
  ansiPretty = dullyellow . integer

instance AnsiPretty Int where
  ansiPretty = dullyellow . int

instance AnsiPretty Doc where
  ansiPretty = id

instance AnsiPretty Bool where
  ansiPretty True = dullyellow $ string "True"
  ansiPretty False = dullyellow $ string "False"

instance AnsiPretty Char where
  ansiPretty c = string [c]
  ansiPrettyList = string

instance AnsiPretty a => AnsiPretty [a] where
  ansiPretty = ansiPrettyList

instance AnsiPretty a => AnsiPretty (Maybe a) where
  ansiPretty (Just x) = ansiPretty x
  ansiPretty Nothing  = dullcyan (string "Nothing")

instance AnsiPretty Text where
  ansiPretty = ansiPretty . T.unpack

instance AnsiPretty UTCTime where
  ansiPretty = ansiPretty . show

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


gprettyWith :: forall a. (Generic a, HasDatatypeInfo a, All2 AnsiPretty (Code a)) => PrettyOpts -> a -> Doc
gprettyWith opts x = gprettyS opts (from x) (datatypeInfo (Proxy :: Proxy a))

gpretty :: forall a. (Generic a, HasDatatypeInfo a, All2 AnsiPretty (Code a)) => a -> Doc
gpretty = gprettyWith defPrettyOpts

gprettyS :: (All2 AnsiPretty xss) => PrettyOpts -> SOP I xss -> DatatypeInfo xss -> Doc
gprettyS _opts (SOP (Z (I x :* Nil))) (Newtype _ _ _)  = ansiPretty x
gprettyS  opts (SOP (Z xs)) (ADT _ _ (ci :* Nil)) = poPrettyRecord opts (constructorName ci) (gprettyP opts xs (fieldInfo ci))
gprettyS _opts (SOP (Z _ )) _ = error "gprettyS: redundant Z case"
gprettyS  opts (SOP (S xss)) (ADT m d (_ :* cis)) = gprettyS opts (SOP xss) (ADT m d cis)
gprettyS _opts (SOP (S _)) _  = error "gprettyS: redundant S case"

gprettyP :: (All AnsiPretty xs) => PrettyOpts -> NP I xs -> NP FieldInfo xs -> [Doc]
gprettyP _opts Nil Nil = []
gprettyP  opts (I x :* xs) (FieldInfo fieldName :* fis) = poPrettyField opts fieldName (ansiPretty x) : gprettyP opts xs fis
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
