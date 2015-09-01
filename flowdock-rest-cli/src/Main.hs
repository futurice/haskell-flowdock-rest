{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
module Main (main) where

import Control.Monad
import Data.Monoid
import Network.HTTP.Client.TLS
import Network.HTTP.Client
import Data.Aeson
import Data.ByteString.Lazy as LBS
import Data.Aeson.Encode.Pretty
import Data.Char
import Options.Applicative

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
  { cmdUrl :: ApiUrl a
  , cmdJson :: Bool
  }

data SomeCommand where
  SomeCommand :: (FromJSON a, Show a) => Command a -> SomeCommand

throwDecode :: FromJSON a => LBS.ByteString -> IO a
throwDecode bs = case eitherDecode bs of
  Right x   -> return x
  Left err  -> error $ "throwDecode: " <> err

commandIO :: forall a. (FromJSON a, Show a) => Command a -> IO ()
commandIO (Command url outputJson) = do
  token <- readAuthToken
  mgr <- newManager tlsManagerSettings
  req <- parseApiUrl url
  let req' = authenticateRequest token req
  res <- httpLbs req' mgr
  -- print res
  if outputJson
    then do jsonRes <- throwDecode (responseBody res) :: IO Value
            LBS.putStr $ encodePretty jsonRes
    else do valueRes <- throwDecode (responseBody res) :: IO a
            print valueRes

paramArgument :: Mod ArgumentFields String -> Parser (ParamName a)
paramArgument mod = mkParamName <$> strArgument mod

commands :: Parser SomeCommand
commands = subparser $ mconcat
  [ command "flows"         (info (helper <*> (SomeCommand <$> flowsCmd))         (progDesc "List flows"))
  , command "all-flows"     (info (helper <*> (SomeCommand <$> allFlowsCmd))      (progDesc "List all flows"))
  , command "flow"          (info (helper <*> (SomeCommand <$> flowCmd)     )     (progDesc "Get a flow"))
  , command "users"         (info (helper <*> (SomeCommand <$> usersCmd))         (progDesc "List all users"))
  , command "flow-users"    (info (helper <*> (SomeCommand <$> flowUsersCmd))     (progDesc "List flow users"))
  , command "org-users"     (info (helper <*> (SomeCommand <$> orgUsersCmd))      (progDesc "List an organization's users"))
  , command "organisations" (info (helper <*> (SomeCommand <$> organisationsCmd)) (progDesc "List organisations"))
  , command "organisation"  (info (helper <*> (SomeCommand <$> organisationCmd))  (progDesc "Get an organisation"))
  ]
  where
    mkCmd :: Parser (ApiUrl a) -> Parser (Command a)
    mkCmd urlParser = Command <$> urlParser
                              <*> switch (long "json" <> help "Whether to output raw json")

    flowsCmd = mkCmd (pure flowsUrl)
    allFlowsCmd = mkCmd (pure allFlowsUrl)
    flowCmd = mkCmd (flowGetUrl <$> paramArgument (metavar "ORG") <*> paramArgument (metavar "FLOW"))
    usersCmd = mkCmd (pure usersUrl)
    flowUsersCmd = mkCmd (flowUsersUrl <$> paramArgument (metavar "ORG") <*> paramArgument (metavar "FLOW"))
    orgUsersCmd = mkCmd (orgUsersUrl <$> paramArgument (metavar "ORG"))
    organisationsCmd = mkCmd (pure organisationsUrl)
    organisationCmd = mkCmd (organisationUrl <$> paramArgument (metavar "ORG"))

main' :: SomeCommand -> IO ()
main' (SomeCommand cmd) = commandIO cmd

main :: IO ()
main = execParser opts >>= main'
  where
    opts = info (helper <*> commands)
      ( fullDesc
     <> progDesc "Try --help if unsure"
     <> header "flowdock-rest-cli - a test for flowdock-rest" )
