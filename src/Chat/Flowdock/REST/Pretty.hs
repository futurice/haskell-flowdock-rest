-- | This module should probably live somewhere else, contains helpers to define 'Pretty' instances.
module Chat.Flowdock.REST.Pretty (module PP, prettyField, prettyText, prettyFunctorText, prettyRecord) where

import Data.Text as T
import Text.PrettyPrint.ANSI.Leijen as PP hiding ((<>), (<$>))

prettyField :: Pretty a => String -> a -> Doc
prettyField name value = text name <+> equals <+> pretty value

prettyRecord :: String -> [Doc] -> Doc
prettyRecord name fields = hang 2 (text name </> semiBraces fields)

prettyText :: Text -> Doc
prettyText = pretty . T.unpack

prettyFunctorText :: (Functor f, Pretty (f String)) => f Text -> Doc
prettyFunctorText = pretty . fmap T.unpack
