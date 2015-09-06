{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DefaultSignatures #-}
-- | This module should probably live somewhere else (in own package).
module Text.PrettyPrint.ANSI.Leijen.AnsiPretty (
  -- * Class
  AnsiPretty(..),
  -- * Generic
  gAnsiPretty,
  gAnsiPrettyWith,
  AnsiPrettyOpts(..),
  defAnsiPrettyOpts,
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
  
  default ansiPretty :: (Generic a, HasDatatypeInfo a, All2 AnsiPretty (Code a)) => a -> Doc
  ansiPretty = gAnsiPretty

  ansiPrettyList :: [a] -> Doc
  ansiPrettyList = encloseSep (dullgreen lbracket) (dullgreen rbracket) (dullgreen colon) . fmap ansiPretty

semiBraces :: [Doc] -> Doc
semiBraces = encloseSep (dullblue lbrace) (dullblue rbrace) (dullblue semi)

prettyField :: AnsiPretty a => String -> a -> Doc
prettyField name value = black (text name) <+> blue equals <+> ansiPretty value

prettyRecord :: String -> [Doc] -> Doc
prettyRecord name fields = hang 2 (cyan (text name) </> semiBraces fields)

data AnsiPrettyOpts = AnsiPrettyOpts
  { poPrettyField :: FieldName -> Doc -> Doc
  , poPrettyRecord :: ConstructorName -> [Doc] -> Doc
  }

defAnsiPrettyOpts :: AnsiPrettyOpts
defAnsiPrettyOpts = AnsiPrettyOpts prettyField prettyRecord

-- | 'PrettyOpts' used in @flowdock-rest@
prettyOpts :: String -> AnsiPrettyOpts
prettyOpts prefix = defAnsiPrettyOpts { poPrettyField = poPrettyField defAnsiPrettyOpts . renamer }
  where renamer name| prefix `L.isPrefixOf` name  = dropTrailingPrime . lowerFirst .  Prelude.drop prefixLen $ name
                    | otherwise                   = name
        prefixLen = Prelude.length prefix
        lowerFirst (x:xs) = C.toLower x : xs
        lowerFirst xs     = xs
        dropTrailingPrime []      = []
        dropTrailingPrime ['\'']  = []
        dropTrailingPrime (x:xs)  = x : dropTrailingPrime xs


gAnsiPrettyWith :: forall a. (Generic a, HasDatatypeInfo a, All2 AnsiPretty (Code a)) => AnsiPrettyOpts -> a -> Doc
gAnsiPrettyWith opts x = gAnsiPrettyS opts (from x) (datatypeInfo (Proxy :: Proxy a))

gAnsiPretty :: forall a. (Generic a, HasDatatypeInfo a, All2 AnsiPretty (Code a)) => a -> Doc
gAnsiPretty = gAnsiPrettyWith defAnsiPrettyOpts

gAnsiPrettyS :: (All2 AnsiPretty xss) => AnsiPrettyOpts -> SOP I xss -> DatatypeInfo xss -> Doc
gAnsiPrettyS _opts (SOP (Z (I x :* Nil))) (Newtype _ _ _)  = ansiPretty x
gAnsiPrettyS  opts (SOP (Z xs)) (ADT _ _ (ci :* Nil)) = poPrettyRecord opts (constructorName ci) (gAnsiPrettyP opts xs (fieldInfo ci))
gAnsiPrettyS _opts (SOP (Z _ )) _ = error "gAnsiPrettyS: redundant Z case"
gAnsiPrettyS  opts (SOP (S xss)) (ADT m d (_ :* cis)) = gAnsiPrettyS opts (SOP xss) (ADT m d cis)
gAnsiPrettyS _opts (SOP (S _)) _  = error "gAnsiPrettyS: redundant S case"

gAnsiPrettyP :: (All AnsiPretty xs) => AnsiPrettyOpts -> NP I xs -> NP FieldInfo xs -> [Doc]
gAnsiPrettyP _opts Nil Nil = []
gAnsiPrettyP  opts (I x :* xs) (FieldInfo fieldName :* fis) = poPrettyField opts fieldName (ansiPretty x) : gAnsiPrettyP opts xs fis
gAnsiPrettyP _opts _ _ = error "gAnsiPrettyP: redundant case"

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

-- Instances

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
