{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}
-- |
-- Module      : Chat.Flowdock.REST.User
-- License     : BSD3
-- Maintainer  : Oleg Grenrus <oleg.grenrus@iki.fi>
module Chat.Flowdock.REST.User
  ( User(..)
  , UserId
  , UserLike(..)
  ) where

import Prelude ()
import Prelude.Compat

import Control.DeepSeq
import Control.Lens
import Data.Aeson.Compat
import Data.Binary.Orphans
import Data.Binary.Tagged
import Data.Hashable
import Data.Text
import Data.Typeable                           (Typeable)
import Generics.SOP                            as SOP
import GHC.Generics                            as GHC
import Text.PrettyPrint.ANSI.Leijen.AnsiPretty

import Chat.Flowdock.REST.Internal

-- | Opaque User identifier
type UserId = Identifier Word64 User

data User = User
  { _userId'      :: !UserId
  , _userEmail'   :: !Text
  , _userName'    :: !Text
  , _userNick'    :: !Text
  , _userAvatar'  :: !Text
  , _userWebsite' :: !(Maybe Text)
  }
  deriving (Eq, Ord, Show, GHC.Generic, Typeable)

makeLenses ''User

instance NFData User
instance Hashable User
instance SOP.Generic User
instance SOP.HasDatatypeInfo User
instance Binary User
instance HasStructuralInfo User
instance HasSemanticVersion User

instance FromJSON User where
  parseJSON = withObject "User" $ \obj ->
    User <$> obj .: "id"
         <*> obj .: "email"
         <*> obj .: "name"
         <*> obj .: "nick"
         <*> obj .: "avatar"
         <*> obj .: "website"

instance AnsiPretty User

-- | 'User' like structures.
class UserLike u where
  userId       :: Lens' u UserId
  userName     :: Lens' u Text
  userEmail    :: Lens' u Text
  userNick     :: Lens' u Text
  userAvatar   :: Lens' u Text
  userWebsite  :: Lens' u (Maybe Text)

instance UserLike User where
  userId       = userId'
  userEmail    = userEmail'
  userName     = userName'
  userNick     = userNick'
  userAvatar   = userAvatar'
  userWebsite  = userWebsite'

