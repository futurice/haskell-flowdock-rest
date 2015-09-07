{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
module Main (main) where

import Control.Lens
import Control.Monad
import Control.Monad.Caching
import Control.Monad.Catch
import Control.Monad.HTTP
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.ByteString.Lazy as LBS
import Data.Char
import Data.Maybe (fromJust)
import Data.Monoid
import Data.Tagged
import Network.HTTP.Client hiding (httpLbs)
import Network.HTTP.Client.TLS
import Options.Applicative

import Chat.Flowdock.REST
import Text.PrettyPrint.ANSI.Leijen.AnsiPretty

stringTrim :: String -> String
stringTrim = r . ltrim . r . ltrim
  where ltrim = Prelude.dropWhile isSpace
        r = Prelude.reverse

readAuthToken :: IO AuthToken
readAuthToken = do
  contents <- Prelude.readFile ".flowdock-rest-api-token"
  return $ AuthToken $ stringTrim contents

data Command a = Command
  { cmdRequest :: Tagged a Request
  , cmdJson :: Bool
  }

data SomeCommand where
  SomeCommand :: (FromJSON a, AnsiPretty a) => Command a -> SomeCommand

throwDecode :: (MonadThrow m, FromJSON a) => LBS.ByteString -> m a
throwDecode bs = case eitherDecode bs of
  Right x   -> return x
  Left err  -> error $ "throwDecode: " <> err

commandM :: forall a m . (FromJSON a, AnsiPretty a, MonadThrow m, MonadIO m, MonadHTTP m) => Command a -> m ()
commandM (Command req outputJson) = do
  token <- liftIO readAuthToken
  let req' = authenticateRequest token $ unTagged req
  res <- httpLbs req'
  -- print res
  if outputJson
    then do jsonRes <- throwDecode (responseBody res) :: m Value
            liftIO $ LBS.putStr $ encodePretty jsonRes
    else do valueRes <- throwDecode (responseBody res) :: m a
            liftIO $ putDoc $ ansiPretty valueRes
            liftIO $ Prelude.putChar '\n'

httpIO :: CachingT (HttpT IO) a -> IO a
httpIO a = do
  mgr <- newManager tlsManagerSettings
  flip runHttpT mgr . flip runCachingT (responseDirectoryCache ".cache") $ a

paramArgument :: Mod ArgumentFields String -> Parser (ParamName a)
paramArgument mod = mkParamName <$> strArgument mod

commands :: Parser SomeCommand
commands = subparser $ mconcat
  [ command "flows"         (info (helper <*> (SomeCommand <$> flowsCmd))         (progDesc "List flows"))
  , command "all-flows"     (info (helper <*> (SomeCommand <$> allFlowsCmd))      (progDesc "List all flows"))
  , command "flow"          (info (helper <*> (SomeCommand <$> flowCmd))          (progDesc "Get a flow"))
  , command "messages"      (info (helper <*> (SomeCommand <$> messagesCmd))      (progDesc "List messages"))
  , command "users"         (info (helper <*> (SomeCommand <$> usersCmd))         (progDesc "List all users"))
  , command "flow-users"    (info (helper <*> (SomeCommand <$> flowUsersCmd))     (progDesc "List flow users"))
  , command "org-users"     (info (helper <*> (SomeCommand <$> orgUsersCmd))      (progDesc "List an organization's users"))
  , command "organisations" (info (helper <*> (SomeCommand <$> organisationsCmd)) (progDesc "List organisations"))
  , command "organisation"  (info (helper <*> (SomeCommand <$> organisationCmd))  (progDesc "Get an organisation"))
  ]
  where
    mkCmd :: Parser (ApiUrl a) -> Parser (Command a)
    mkCmd urlParser = mkCmd' requestParser
      where requestParser = parseApiUrl <$> urlParser

    mkCmd' :: Parser (Maybe (Tagged a Request)) -> Parser (Command a)
    mkCmd' requestParser = Command <$> (fromJust <$> requestParser)
                                   <*> switch (long "json" <> help "Whether to output raw json")

    flowsCmd          = mkCmd (pure flowsUrl)
    allFlowsCmd       = mkCmd (pure allFlowsUrl)
    flowCmd           = mkCmd (flowGetUrl <$> paramArgument (metavar "ORG") <*> paramArgument (metavar "FLOW"))
    messagesCmd       = mkCmd' (messagesRequest <$> paramArgument (metavar "ORG") <*> paramArgument (metavar "FLOW") <*> parseMessageOptions)
      where parseMessageOptions = messageOptions <$> optional (option (eitherReader ev) (long "event" <> metavar "EVENT" <> help "Filter messages by event type."))
                                                 <*> optional (option auto (long "limit" <> metavar "LIMIT" <> help "Maximum number of messages to return."))
                                                 <*> optional (option auto (long "until" <> metavar "MSGID" <> help "Get messages leading to a message id."))
            ev e = maybe (Left ("Unknown event: " <> e)) Right (messageEventFromString e)
    usersCmd          = mkCmd (pure usersUrl)
    flowUsersCmd      = mkCmd (flowUsersUrl <$> paramArgument (metavar "ORG") <*> paramArgument (metavar "FLOW"))
    orgUsersCmd       = mkCmd (orgUsersUrl <$> paramArgument (metavar "ORG"))
    organisationsCmd  = mkCmd (pure organisationsUrl)
    organisationCmd   = mkCmd (organisationUrl <$> paramArgument (metavar "ORG"))


messageOptions :: Maybe MessageEvent -> Maybe Int -> Maybe Integer -> MessageOptions
messageOptions event limit untilId =
  defMessageOptions & msgOptEvent .~ event
                    & msgOptLimit .~ limit
                    & msgOptUntilId .~ (mkIdentifier <$> untilId)

main' :: SomeCommand -> IO ()
main' (SomeCommand cmd) = httpIO $ commandM cmd

main :: IO ()
main = execParser opts >>= main'
  where
    opts = info (helper <*> commands)
      ( fullDesc
     <> progDesc "Try --help if unsure"
     <> header "flowdock-rest-cli - a test for flowdock-rest" )
