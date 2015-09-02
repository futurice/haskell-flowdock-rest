{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Chat.Flowdock.REST.User 
  ( User(..)
  , UserLike(..)
  ) where

import Control.Applicative
import Control.DeepSeq
import Control.Lens
import Data.Aeson
import Data.Hashable
import Data.Text
import GHC.Generics
import Text.PrettyPrint.ANSI.Leijen hiding ((<>), (<$>))

import Chat.Flowdock.REST.Internal

data User = User
  { _userId' :: !UserId
  , _userEmail' :: !Text
  , _userName' :: !Text
  , _userNick' :: !Text
  , _userAvatar' :: !Text
  , _userWebsite' :: !(Maybe Text)
  }
  deriving (Eq, Ord, Show, Generic)

makeLenses ''User

instance NFData User
instance Hashable User

instance Pretty User where
  pretty = text . show

instance FromJSON User where
  parseJSON = withObject "User" $ \obj ->
    User <$> obj .: "id"
         <*> obj .: "email"
         <*> obj .: "name"
         <*> obj .: "nick"
         <*> obj .: "avatar"
         <*> obj .: "website"

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

