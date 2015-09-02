{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Chat.Flowdock.REST.Message where

import Data.Aeson

import Control.Applicative
import Control.DeepSeq
import Control.Lens
import Data.Aeson
import Data.Hashable
import Data.Monoid
import Data.Text as T
import Data.Time
import GHC.Generics
import Text.PrettyPrint.ANSI.Leijen hiding ((<>), (<$>))

import Chat.Flowdock.REST.Internal
import Chat.Flowdock.REST.Organisation

data MessageContent = MTMessage String
                    | MTStatus
                    | MTComment Value
                    | MTAction
                    | MTTagChange
                    | MTMessageEdit
                    | MTActivityUser
                    | MTFile Value
                    | MTMail Value
                    | MTActivity
                    | MTDiscussion Value
  deriving (Eq, Show, Generic)

instance NFData MessageContent
instance Hashable MessageContent

instance FromJSON MessageContent where
  parseJSON = withObject "Message" $ \obj -> do
    event <- obj .: "event"
    content <- obj .: "content"
    case event of
      "message" -> MTMessage <$> parseJSON content
      "comment" -> pure $ MTComment content
      "file"    -> pure $ MTFile content
      "mail"    -> pure $ MTMail content
      _         -> fail $ "Invalid message type: " <> event

data Message = Message
  { _msgContent :: MessageContent
  , _msgTags :: [Text]
  , _msgCreatedAt :: UTCTime
  , _msgEditedAt :: Maybe UTCTime
  , _msgFlowId :: FlowId
  , _msgId :: MessageId
  }
  deriving (Eq, Show, Generic)

makeLenses ''Message

instance NFData Message
-- instance Hashable Message

instance FromJSON Message where
  parseJSON v = do
    content <- parseJSON v
    flip (withObject "Message") v $ \obj -> 
      Message <$> pure content
              <*> obj .: "tags"
              <*> obj .: "created_at"
              <*> obj .:? "edited_at"
              <*> obj .: "flow"
              <*> obj .: "id"

instance Pretty Message where
  pretty = text . show
