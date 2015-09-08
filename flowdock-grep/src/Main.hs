{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Data.Aeson
import Data.Binary.Orphans
import Data.Binary.Tagged
import Data.ByteString.Lazy as LBS
import Data.Char
import Data.List as L
import Data.Maybe (isNothing, mapMaybe)
import Data.Monoid
import Data.Tagged
import Data.Text as T
import Data.Time
import Data.Typeable
import GHC.Generics
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Options.Applicative
import Path
import System.Directory
import System.Environment

import Chat.Flowdock.REST

stringTrim :: String -> String
stringTrim = r . ltrim . r . ltrim
  where ltrim = Prelude.dropWhile isSpace
        r = Prelude.reverse

data NoUserDirectory = NoUserDirectory
  deriving (Show, Typeable)

instance Exception NoUserDirectory

tokenRelPath :: Path Rel File
tokenRelPath = $(mkRelFile "auth-token")

settingsRelPath :: Path Rel Dir
settingsRelPath = $(mkRelDir ".flowdock-grep")

{-
flowsRelPath :: Path Rel Dir
flowsRelPath = $(mkRelDir "flows")

usersRelPath :: Path Rel Dir
usersRelPath = $(mkRelDir "users")
-}

lookupSettingsDirectory :: IO (Path Abs Dir)
lookupSettingsDirectory = do
  home <- lookupEnv "HOME"
  user <- lookupEnv "USER"
  case (home <|> user) of
    Just dir -> (</> settingsRelPath) <$> parseAbsDir dir
    Nothing  -> throwM NoUserDirectory

readAuthToken :: Path Abs Dir -> IO (Maybe AuthToken)
readAuthToken dir = readAuthToken' `catch` onIOErrorNothing
  where
    onIOErrorNothing :: IOError -> IO (Maybe AuthToken)
    onIOErrorNothing _ = return Nothing
    readAuthToken' = do
      contents <- Prelude.readFile (toFilePath (dir </> tokenRelPath))
      return $ Just $ AuthToken $ stringTrim contents

writeAuthToken :: Path Abs Dir -> AuthToken -> IO ()
writeAuthToken dir token = do
  let path = toFilePath (dir </> tokenRelPath)
  createDirectoryIfMissing True (toFilePath dir)
  Prelude.writeFile path (getAuthToken token)

data Opts = Opts
  { optsToken :: AuthToken
  , _optsOffline :: Bool
  , optsOrganisation :: ParamName Organisation
  , optsFlow :: ParamName Flow
  , optsNeedle :: String
  }
  deriving Show

paramArgument :: Mod ArgumentFields String -> Parser (ParamName a)
paramArgument m = mkParamName <$> strArgument m

authTokenParser :: Maybe AuthToken -> Parser AuthToken
authTokenParser maybeToken =
    option (eitherReader er) (long "token" <> metavar "token" <> help "Flowdock authentication token: see https://flowdock.com/account/tokens" <> def)
  where
    def = maybe idm value maybeToken
    er = Right . AuthToken

optsParser :: Maybe AuthToken -> Parser Opts
optsParser maybeToken =
  Opts <$> authTokenParser maybeToken
       <*> switch (long "offline" <> help "Consider only already downloaded logs")
       <*> paramArgument (metavar "org" <> help "Organisation slug, check it from the web url: wwww.flowdock.com/app/ORG/FLOW/")
       <*> paramArgument (metavar "flow" <> help "Flow slug")
       <*> fmap (L.intercalate " ") (many $ strArgument (metavar "needle"))

baseMessageOptions :: Maybe Integer -> MessageOptions
baseMessageOptions sinceId =
  defMessageOptions & msgOptEvents   .~ [EventMessage, EventComment]
                    & msgOptLimit    .~ Just 100
                    & msgOptSinceId  .~ (mkIdentifier <$> sinceId)
                    & msgOptSorting  .~ Ascending

data Row = Row
  { rowUser       :: UserId
  , rowCreatedAt  :: UTCTime
  , rowText       :: Text
  }
  deriving (Eq, Ord, Show, Generic)

messageToRow :: Message -> Maybe Row
messageToRow msg = Row (msg ^. msgUser) (msg ^. msgCreatedAt) <$> messageContentToRow (msg ^. msgContent)
  where messageContentToRow (MTMessage text)  = Just text
        messageContentToRow (MTComment comm)  = Just (comm ^. commentText)
        messageContentToRow _                 = Nothing

grepRow :: Text -> [Row] -> IO ()
grepRow needle rows = mapM_ p rows
  where p row = when (needle `T.isInfixOf` (rowText row))
                     (print row)

main' :: Path Abs Dir -> Bool -> Opts -> IO ()
main' settingsDirectory writeToken opts = do
  let token = optsToken opts
  when writeToken $ writeAuthToken settingsDirectory token
  print opts
  mgr <- newManager tlsManagerSettings
  req <- untag <$> messagesRequest (optsOrganisation opts) (optsFlow opts) (baseMessageOptions Nothing)
  let req' = authenticateRequest token req
  res <- httpLbs req' mgr
  rows <- mapMaybe messageToRow <$> throwDecode (responseBody res) :: IO [Row]
  grepRow (T.pack $ optsNeedle opts) rows

main :: IO ()
main = do
  settingsDirectory <- lookupSettingsDirectory
  token <- readAuthToken settingsDirectory
  let opts = info (helper <*> optsParser token) (fullDesc <> progDesc "Try --help if unsure" <> header "flowdock-grep - grep flowdock logs")
  execParser opts >>= main' settingsDirectory (isNothing token)

-- Helpers
throwDecode :: (MonadThrow m, FromJSON a) => LBS.ByteString -> m a
throwDecode bs = case eitherDecode bs of
  Right x   -> return x
  Left err  -> error $ "throwDecode: " <> err
