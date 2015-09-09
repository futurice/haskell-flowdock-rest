{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
-- |
-- Module      : Chat.Flowdock.REST.Message
-- License     : BSD3
-- Maintainer  : Oleg Grenrus <oleg.grenrus@iki.fi>
module Chat.Flowdock.REST.Message (
  -- * Message
  Message(..),
  MessageId,
  msgContent,
  msgTags,
  msgCreatedAt,
  msgEditedAt,
  msgFlowId,
  msgId,
  msgUser,
  -- * Content
  MessageEvent(..),
  messageEventToString,
  messageEventFromString,
  MessageContent(..),
  _MTStatus,
  _MTComment,
  _MTAction,
  _MTTagChange,
  _MTMessageEdit,
  _MTActivityUser,
  _MTFile,
  _MTMail,
  _MTActivity,
  _MTDiscussion,
  -- * Comment
  Comment(..),
  commentText,
  commentTitle,
  -- * Mail
  Mail(..),
  mailSubject,
  mailContent,
  mailFrom,
  mailTo,
  mailCc,
  mailBcc,
  mailReplyTo,
  MailAddress(..),
  mailAddress,
  mailAddressName,
  ) where

import Control.Applicative
import Control.DeepSeq
import Control.Lens
import Data.Aeson
import Data.Binary.Orphans
import Data.Binary.Tagged
import Data.Hashable
import Data.Monoid
import Data.Text as T
import Data.Time
import GHC.Generics as GHC
import Generics.SOP as SOP
import Text.PrettyPrint.ANSI.Leijen.AnsiPretty

import Chat.Flowdock.REST.Internal
import Chat.Flowdock.REST.User
import Chat.Flowdock.REST.Flow

data Comment = Comment
  { _commentText  :: !Text
  , _commentTitle :: !Text
  }
  deriving (Eq, Ord, Show, GHC.Generic)

makeLenses ''Comment

instance NFData Comment
instance Hashable Comment
instance SOP.Generic Comment
instance SOP.HasDatatypeInfo Comment
instance Binary Comment
instance HasStructuralInfo Comment
instance HasSemanticVersion Comment

instance FromJSON Comment where
  parseJSON = withObject "Comment" $ \obj ->
    Comment <$> obj .: "text"
            <*> obj .: "title"

instance AnsiPretty Comment where
  ansiPretty = gAnsiPrettyWith (prettyOpts "_comment")

data MailAddress = MailAddress
  { _mailAddress     :: !Text
  , _mailAddressName :: !(Maybe Text)
  }
  deriving (Eq, Ord, Show, GHC.Generic)

makeLenses ''MailAddress

instance NFData MailAddress
instance Hashable MailAddress
instance SOP.Generic MailAddress
instance SOP.HasDatatypeInfo MailAddress
instance Binary MailAddress

instance FromJSON MailAddress where
  parseJSON = withObject "MailAddress" $ \obj ->
    MailAddress <$> obj .: "address"
                <*> obj .:? "name"

instance AnsiPretty MailAddress where
  ansiPretty (MailAddress addr Nothing)     = text . T.unpack $ addr
  ansiPretty (MailAddress addr (Just name)) = text . T.unpack $ name <> " <" <> addr <> ">"


data Mail = Mail
  { _mailSubject :: !Text
  , _mailContent :: !Text
  , _mailTo      :: ![MailAddress]
  , _mailFrom    :: ![MailAddress]
  , _mailCc      :: ![MailAddress]
  , _mailBcc     :: ![MailAddress]
  , _mailReplyTo :: ![MailAddress]
  }
  deriving (Eq, Ord, Show, GHC.Generic)

makeLenses ''Mail

instance NFData Mail
instance Hashable Mail
instance SOP.Generic Mail
instance SOP.HasDatatypeInfo Mail
instance Binary Mail

instance FromJSON Mail where
  parseJSON = withObject "Mail" $ \obj ->
    Mail <$> obj .: "subject"
         <*> obj .: "content"
         <*> obj .: "to"
         <*> obj .: "from"
         <*> obj .: "cc"
         <*> obj .: "bcc"
         <*> obj .: "replyTo"

instance AnsiPretty Mail where
  ansiPretty = gAnsiPrettyWith (prettyOpts "_mail")

data MessageEvent = EventMessage
                  | EventStatus
                  | EventComment  -- ^ This message type is likely to change in the near future.
                  | EventAction
                  | EventTagChange
                  | EventMessageEdit
                  | EventActivityUser
                  | EventFile
                  | EventMail
                  | EventActivity
                  | EventDiscussion
  deriving (Eq, Ord, Show, Enum, Bounded)

messageEventToString :: MessageEvent -> String
messageEventToString EventMessage      = "message"
messageEventToString EventStatus       = "status"
messageEventToString EventComment      = "comment"
messageEventToString EventAction       = "action"
messageEventToString EventTagChange    = "tag-change"
messageEventToString EventMessageEdit  = "message-edit"
messageEventToString EventActivityUser = "activity.user"
messageEventToString EventFile         = "file"
messageEventToString EventMail         = "mail"
messageEventToString EventActivity     = "activity"
messageEventToString EventDiscussion   = "discussion"

messageEventLookupTable :: [(String, MessageEvent)]
messageEventLookupTable = fmap f [minBound..maxBound]
  where f e = (messageEventToString e, e)

messageEventFromString :: String -> Maybe MessageEvent
messageEventFromString = flip lookup messageEventLookupTable

data MessageContent = MTMessage !Text
                    | MTStatus String
                    | MTComment !Comment -- ^ This message type is likely to change in the near future.
                    | MTAction !Value
                    | MTTagChange !Value
                    | MTMessageEdit !Value
                    | MTActivityUser !Value
                    | MTFile !Value
                    | MTMail !Mail
                    | MTActivity -- No action
                    | MTDiscussion
  deriving (Eq, Show, GHC.Generic)

instance NFData MessageContent
instance Hashable MessageContent
instance SOP.Generic MessageContent
instance SOP.HasDatatypeInfo MessageContent
instance Binary MessageContent

makePrisms ''MessageContent

instance FromJSON MessageContent where
  parseJSON = withObject "Message" $ \obj -> do
    event <- obj .: "event"
    content <- obj .: "content"
    case event of
      "message"    -> MTMessage    <$> parseJSON content
      "comment"    -> MTComment    <$> parseJSON content
      "status"     -> MTStatus     <$> parseJSON content
      "action"     -> MTAction     <$> parseJSON content
      "file"       -> MTFile       <$> parseJSON content
      "mail"       -> MTMail       <$> parseJSON content
      "activity"   -> pure MTActivity
      "discussion" -> pure MTDiscussion
      _          -> fail $ "Invalid message type: " <> event


instance AnsiPretty MessageContent where
  ansiPretty (MTMessage msg) = text "message:" <+> ansiPretty msg
  ansiPretty (MTComment com) = ansiPretty com
  ansiPretty (MTMail mail)   = ansiPretty mail
  ansiPretty m = text . show $ m

data Message = Message
  { _msgContent    :: !MessageContent
  , _msgTags       :: ![Text]
  , _msgCreatedAt  :: !UTCTime
  , _msgEditedAt   :: !(Maybe UTCTime)
  , _msgFlowId     :: !FlowId
  , _msgUser       :: !UserId
  , _msgId         :: !MessageId
  }
  deriving (Eq, Show, GHC.Generic)

-- | Opaque User identifier
type MessageId = Identifier Integer Message

makeLenses ''Message

instance NFData Message
-- instance Hashable Message
instance SOP.Generic Message
instance SOP.HasDatatypeInfo Message
instance Binary Message

instance FromJSON Message where
  parseJSON v = do
    content <- parseJSON v
    flip (withObject "Message") v $ \obj ->
      Message <$> pure content
              <*> obj .: "tags"
              <*> obj .: "created_at"
              <*> obj .:? "edited_at"
              <*> obj .: "flow"
              <*> (mkIdentifier . read <$> obj.: "user") -- User field is string, in future there might be integral `user_id` field.
              <*> obj .: "id"

instance AnsiPretty Message where
  ansiPretty = gAnsiPrettyWith (prettyOpts "_msg")
