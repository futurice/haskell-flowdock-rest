{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
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

import Chat.Flowdock.REST.Internal
import Chat.Flowdock.REST.Organisation
import Chat.Flowdock.REST.Pretty

data Comment = Comment
  { _commentText :: !Text
  , _commentTitle :: !Text
  }
  deriving (Eq, Ord, Show, Generic)

instance NFData Comment
instance Hashable Comment

instance FromJSON Comment where
  parseJSON = withObject "Commnet" $ \obj ->
    Comment <$> obj .: "text"
            <*> obj .: "title"

instance Pretty Comment where
  pretty Comment {..} = text "Comment" </> semiBraces
    [ prettyField "text" (T.unpack _commentText)
    , prettyField "title" (T.unpack _commentTitle)
    ]

{-
MTMail (Object (fromList [("subject",String "Keyper - 6 hours"),("bcc",Array (fromList [])),("sender",Null),("to",Array (fromList [Object (fromList [("address",String "open-source@futurice.flowdock.com"),("name",Null)])])),("from",Array (fromList [Object (fromList [("address",String "neil.mcalister@futurice.com"),("name",String "Neil McAlister")])])),("replyTo",Array (fromList [])),("content",String "<div dir=\"ltr\">Making a little app in my spare time so I can securely lug my bank codes around.<br><div>\n<br>#todo #contribution<br><br>95e4ae3 Remove stray 5<br>2715dfa Add desktop UI, &amp; multiple files feature<br><br><a href=\"https://github.com/pingzing/Keyper/commits/master\">https://github.com/pingzing/Keyper/commits/master</a><br>-- <br><div>\n<div class=\"gmail_signature\"><div dir=\"ltr\"><div><div dir=\"ltr\"><div>Neil McAlister \183 <span>+358 50 464 4090</span><br>Software Developer <span>\183</span> Futurice Oy<br><a href=\"http://www.futurice.com\">www.futurice.com</a> \183 <a href=\"http://twitter.com/futurice\">twitter.com/futurice</a><br><br>\n</div></div></div></div></div>\n</div>\n</div>\n</div>\n"),("cc",Array (fromList [])),("contentType",

-}

data Mail = Mail
  { _mailSubject :: !Text
  , _mailContent :: !Text
  }
  deriving (Eq, Ord, Show, Generic)


instance NFData Mail
instance Hashable Mail

instance FromJSON Mail where
  parseJSON = withObject "Mail" $ \obj ->
    Mail <$> obj .: "subject"
         <*> obj .: "content"

instance Pretty Mail where
  pretty Mail {..} = text "Mail" </> semiBraces
    [ prettyField "subject" (T.unpack _mailSubject)
    , prettyField "content" (T.unpack _mailContent)
    ]

data MessageContent = MTMessage String
                    | MTStatus
                    | MTComment Comment
                    | MTAction
                    | MTTagChange
                    | MTMessageEdit
                    | MTActivityUser
                    | MTFile Value
                    | MTMail Mail
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
      "comment" -> MTComment <$> parseJSON content
      "file"    -> pure $ MTFile content
      "mail"    -> MTMail <$> parseJSON content
      _         -> fail $ "Invalid message type: " <> event


instance Pretty MessageContent where
  pretty (MTMessage msg) = text "message:" <+> text msg
  pretty (MTComment com) = pretty com
  pretty (MTMail mail)   = pretty mail
  pretty m = text . show $ m

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
  pretty Message {..} = text "Message" </> semiBraces
    [ prettyField "id" _msgId
    , prettyField "flow" _msgFlowId
    , prettyField "tags" (T.unpack <$> _msgTags)
    , prettyField "content" _msgContent
    ]
