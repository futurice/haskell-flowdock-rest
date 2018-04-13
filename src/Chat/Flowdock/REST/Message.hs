{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}
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
  _MTActivity,
  _MTDiscussion,
  -- * Comment
  Comment(..),
  commentText,
  commentTitle,
  -- * Discussion
  Discussion(..),
  discussionBody,
  discussionThread,
  discussionTitle,
  discussionAuthor,
  Thread(..),
  threadSource,
  threadId,
  threadTitle,
  Source(..),
  sourceId,
  sourceName,
  sourceApplication,
  Author(..),
  authorName,
  authorAvatar,
  authorEmail,
  Application(..),
  applicationId,
  applicationName,
  ) where

import Prelude        ()
import Prelude.Compat

import Control.Applicative                     (optional, (<|>))
import Control.DeepSeq
import Control.Lens
import Data.Aeson.Compat
import Data.Binary.Orphans
import Data.Binary.Tagged
import Data.Hashable
import Data.Text                               as T
import Data.Time
import Data.Time.Clock.POSIX                   (posixSecondsToUTCTime)
import Data.Typeable                           (Typeable)
import Data.Vector
import Generics.SOP                            as SOP
import GHC.Generics                            as GHC
import Text.PrettyPrint.ANSI.Leijen.AnsiPretty

import Chat.Flowdock.REST.Flow
import Chat.Flowdock.REST.Internal
import Chat.Flowdock.REST.User

data Comment = Comment
  { _commentText  :: !Text
  , _commentTitle :: !Text
  }
  deriving (Eq, Ord, Show, GHC.Generic, Typeable)

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

instance AnsiPretty Comment

data MessageEvent
  = EventMessage
  | EventStatus
  | EventComment
    -- ^ This message type is likely to change in the near future.
  | EventAction
  | EventTagChange
  | EventMessageEdit
  | EventActivityUser
  | EventFile
  | EventActivity
  | EventDiscussion
  | EventUserEdit
  deriving (Eq, Ord, Show, Enum, Bounded, Typeable)

messageEventToString :: MessageEvent -> String
messageEventToString EventMessage      = "message"
messageEventToString EventStatus       = "status"
messageEventToString EventComment      = "comment"
messageEventToString EventAction       = "action"
messageEventToString EventTagChange    = "tag-change"
messageEventToString EventMessageEdit  = "message-edit"
messageEventToString EventActivityUser = "activity.user"
messageEventToString EventFile         = "file"
messageEventToString EventActivity     = "activity"
messageEventToString EventDiscussion   = "discussion"
messageEventToString EventUserEdit     = "user-edit"

messageEventLookupTable :: [(String, MessageEvent)]
messageEventLookupTable = fmap f [minBound..maxBound]
  where f e = (messageEventToString e, e)

messageEventFromString :: String -> Maybe MessageEvent
messageEventFromString = flip lookup messageEventLookupTable

type ApplicationId = Identifier Integer Application

data Application = Application
    { _applicationName :: !Text
    , _applicationId   :: !ApplicationId
    }
    deriving (Eq, Ord, Show, GHC.Generic, Typeable)

makeLenses ''Application

instance NFData Application
instance Hashable Application
instance SOP.Generic Application
instance SOP.HasDatatypeInfo Application
instance Binary Application
instance AnsiPretty Application

instance FromJSON Application where
    parseJSON = withObject "Application" $ \obj ->
        Application
            <$> obj .: "name"
            <*> obj .: "id"

type SourceId = Identifier Integer Source

data Source = Source
    { _sourceName        :: !Text
    , _sourceId          :: !SourceId
    , _sourceApplication :: !Application
    }
    deriving (Eq, Ord, Show, GHC.Generic, Typeable)

makeLenses ''Source

instance NFData Source
instance Hashable Source
instance SOP.Generic Source
instance SOP.HasDatatypeInfo Source
instance Binary Source
instance AnsiPretty Source

instance FromJSON Source where
    parseJSON = withObject "Source" $ \obj ->
        Source
            <$> obj .: "name"
            <*> obj .: "id"
            <*> obj .: "application"

type ThreadId = Identifier Text Thread

data Thread = Thread
    { _threadTitle  :: !Text
    , _threadId     :: !ThreadId
    , _threadSource :: !Source
    }
    deriving (Eq, Ord, Show, GHC.Generic, Typeable)

makeLenses ''Thread

instance NFData Thread
instance Hashable Thread
instance SOP.Generic Thread
instance SOP.HasDatatypeInfo Thread
instance Binary Thread
instance AnsiPretty Thread

instance FromJSON Thread where
    parseJSON = withObject "Thread" $ \obj ->
        Thread
            <$> obj .: "title"
            <*> obj .: "id"
            <*> obj .: "source"

data Author = Author
    { _authorName   :: !(Maybe Text)
    , _authorEmail  :: !(Maybe Text)
    , _authorAvatar :: !(Maybe Text)
    }
    deriving (Eq, Ord, Show, GHC.Generic, Typeable)

makeLenses ''Author

instance NFData Author
instance Hashable Author
instance SOP.Generic Author
instance SOP.HasDatatypeInfo Author
instance Binary Author
instance AnsiPretty Author

instance FromJSON Author where
    parseJSON = withObject "Author" $ \obj ->
        Author
            <$> obj .:? "name"
            <*> obj .:? "email"
            <*> obj .:? "avatar"

data Discussion = Discussion
    { _discussionTitle  :: !Text
    , _discussionAuthor :: !Author
    , _discussionThread :: !Thread
    , _discussionBody   :: !Text
    }
    deriving (Eq, Ord, Show, GHC.Generic, Typeable)

makeLenses ''Discussion

instance NFData Discussion
instance Hashable Discussion
instance SOP.Generic Discussion
instance SOP.HasDatatypeInfo Discussion
instance Binary Discussion
instance AnsiPretty Discussion

instance FromJSON Discussion where
    parseJSON = withObject "Discussion" $ \obj ->
        Discussion
            <$> obj .: "title"
            <*> obj .: "author"
            <*> obj .: "thread"
            <*> obj .: "full_body"

data MessageContent
  = MTMessage !Text
  | MTStatus String
  | MTComment !Comment
    -- ^ This message type is likely to change in the near future.
  | MTAction !Value
  | MTTagChange !Value
  | MTMessageEdit !Value
  | MTActivityUser !Value
  | MTFile !Value
  | MTActivity -- No action
  | MTDiscussion !(Maybe Discussion)
  | MTLine !Value -- ^ what?
  deriving (Eq, Show, GHC.Generic, Typeable)

instance NFData MessageContent
--instance Hashable MessageContent
instance SOP.Generic MessageContent
instance SOP.HasDatatypeInfo MessageContent
instance Binary MessageContent

makePrisms ''MessageContent

instance FromJSON MessageContent where
  parseJSON = withObject "Message" $ \obj -> do
    event <- obj .: "event"
    content <- obj .: "content"
    case event of
      "message"    -> MTMessage     <$> parseJSON content
      "comment"    -> MTComment     <$> parseJSON content
      "status"     -> MTStatus      <$> parseJSON content
      "action"     -> MTAction      <$> parseJSON content
      "file"       -> MTFile        <$> parseJSON content
      "user-edit"  -> MTMessageEdit <$> parseJSON content
      "line"       -> MTLine        <$> parseJSON content
      "activity"   -> pure MTActivity
      "discussion" -> MTDiscussion  <$> optional (parseJSON (Object obj))
      _          -> fail $ "Invalid message type: " <> event


instance AnsiPretty MessageContent where
  ansiPretty (MTMessage msg)      = text "message:" <+> ansiPretty msg
  ansiPretty (MTComment com)      = ansiPretty com
  ansiPretty (MTDiscussion disc)  = ansiPretty disc
  ansiPretty m = text . show $ m

data Message = Message
  { _msgContent   :: !MessageContent
  , _msgTags      :: !(Vector Tag)
  , _msgCreatedAt :: !UTCTime
  , _msgEditedAt  :: !(Maybe UTCTime)
  , _msgFlowId    :: !FlowId
  , _msgUser      :: !UserId
  , _msgId        :: !MessageId
  }
  deriving (Eq, Show, GHC.Generic, Typeable)

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
              <*> (obj .: "created_at" <|> posixSecondsToUTCTime<$> obj .: "sent")
              <*> obj .:? "edited_at"
              <*> obj .: "flow"
              -- User field is string,
              -- in future there might be integral `user_id` field.
              <*> (mkIdentifier . read <$> obj.: "user")
              <*> obj .: "id"

instance AnsiPretty Message
