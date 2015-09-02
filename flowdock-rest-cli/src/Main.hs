{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
module Main (main) where

import Control.Lens
import Control.Monad
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.ByteString.Lazy as LBS
import Data.Char
import Data.Maybe (fromJust)
import Data.Monoid
import Data.Tagged
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Options.Applicative
import Text.PrettyPrint.ANSI.Leijen hiding ((<>), (<$>))

import Chat.Flowdock.REST

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
  SomeCommand :: (FromJSON a, Pretty a) => Command a -> SomeCommand

throwDecode :: FromJSON a => LBS.ByteString -> IO a
throwDecode bs = case eitherDecode bs of
  Right x   -> return x
  Left err  -> error $ "throwDecode: " <> err

commandIO :: forall a. (FromJSON a, Pretty a) => Command a -> IO ()
commandIO (Command req outputJson) = do
  token <- readAuthToken
  mgr <- newManager tlsManagerSettings
  let req' = authenticateRequest token $ unTagged req
  res <- httpLbs req' mgr
  -- print res
  if outputJson
    then do jsonRes <- throwDecode (responseBody res) :: IO Value
            LBS.putStr $ encodePretty jsonRes
    else do valueRes <- throwDecode (responseBody res) :: IO a
            putDoc $ pretty valueRes
            Prelude.putChar '\n'

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
            ev "mail" = Right EventMail
            ev err    = Left $ "Unknown event: " <> err
    usersCmd          = mkCmd (pure usersUrl)
    flowUsersCmd      = mkCmd (flowUsersUrl <$> paramArgument (metavar "ORG") <*> paramArgument (metavar "FLOW"))
    orgUsersCmd       = mkCmd (orgUsersUrl <$> paramArgument (metavar "ORG"))
    organisationsCmd  = mkCmd (pure organisationsUrl)
    organisationCmd   = mkCmd (organisationUrl <$> paramArgument (metavar "ORG"))

messageOptions :: Maybe MessageEvent -> Maybe Int -> MessageOptions
messageOptions event limit = defMessageOptions & msgOptEvent .~ event
                                               & msgOptLimit .~ limit

main' :: SomeCommand -> IO ()
main' (SomeCommand cmd) = commandIO cmd

main :: IO ()
main = execParser opts >>= main'
  where
    opts = info (helper <*> commands)
      ( fullDesc
     <> progDesc "Try --help if unsure"
     <> header "flowdock-rest-cli - a test for flowdock-rest" )
