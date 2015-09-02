{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
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

import Chat.Flowdock.REST.Internal
import Chat.Flowdock.REST.Pretty

data User = User
  { _userId'      :: !UserId
  , _userEmail'   :: !Text
  , _userName'    :: !Text
  , _userNick'    :: !Text
  , _userAvatar'  :: !Text
  , _userWebsite' :: !(Maybe Text)
  }
  deriving (Eq, Ord, Show, Generic)

makeLenses ''User

instance NFData User
instance Hashable User

instance FromJSON User where
  parseJSON = withObject "User" $ \obj ->
    User <$> obj .: "id"
         <*> obj .: "email"
         <*> obj .: "name"
         <*> obj .: "nick"
         <*> obj .: "avatar"
         <*> obj .: "website"

instance Pretty User where
  pretty User {..} = prettyRecord "OrgUser"
    [ prettyField "id" _userId'
    , prettyField "nick" $ prettyText _userNick'
    , prettyField "name" $ prettyText _userName'
    , prettyField "email" $ prettyText _userEmail'
    , prettyField "avatar" $ prettyText _userAvatar'
    , prettyField "website" $ prettyFunctorText _userWebsite'
    ]

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

