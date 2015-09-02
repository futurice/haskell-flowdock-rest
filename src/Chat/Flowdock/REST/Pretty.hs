-- | This module should probably live somewhere else
module Chat.Flowdock.REST.Pretty (module PP, prettyField) where

import Text.PrettyPrint.ANSI.Leijen as PP hiding ((<>), (<$>))

prettyField :: Pretty a => String -> a -> Doc
prettyField name value = text name <+> equals <+> pretty value
