{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Data.Aeson
import Data.Bifunctor
import Data.Binary.Put
import Data.Binary.Get
import Data.Binary.Orphans
import Data.Binary.Tagged
import Data.ByteString.Lazy as LBS
import Data.Char
import Data.List as L
import Data.Maybe (isNothing, isJust, mapMaybe)
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

flowsRelPath :: Path Rel Dir
flowsRelPath = $(mkRelDir "flows")

usersRelPath :: Path Rel File
usersRelPath = $(mkRelFile "users")

lookupSettingsDirectory :: IO (Path Abs Dir)
lookupSettingsDirectory = do
  home <- lookupEnv "HOME"
  user <- lookupEnv "USER"
  case (home <|> user) of
    Just dir -> (</> settingsRelPath) <$> parseAbsDir dir
    Nothing  -> throwM NoUserDirectory

readAuthToken :: Path Abs Dir -> IO (Maybe AuthToken)
readAuthToken dir = readAuthToken' `catch` onIOError Nothing
  where
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
  , optsOffline :: Bool
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

baseMessageOptions :: Maybe MessageId -> MessageOptions
baseMessageOptions sinceId =
  defMessageOptions & msgOptEvents   .~ [EventMessage, EventComment]
                    & msgOptLimit    .~ Just 100
                    & msgOptSinceId  .~ sinceId
                    & msgOptSorting  .~ Ascending

data Row = Row
  { rowMessageId  :: MessageId
  , rowUser       :: UserId
  , rowCreatedAt  :: UTCTime
  , rowText       :: Text
  }
  deriving (Eq, Ord, Show, Generic)

instance Binary Row
instance HasStructuralInfo Row
instance HasSemanticVersion Row

messageToRow :: Message -> Maybe Row
messageToRow msg = Row (msg ^. msgId) (msg ^. msgUser) (msg ^. msgCreatedAt) <$> messageContentToRow (msg ^. msgContent)
  where messageContentToRow (MTMessage text)  = Just text
        messageContentToRow (MTComment comm)  = Just (comm ^. commentText)
        messageContentToRow _                 = Nothing

parseCachePath :: Path Abs Dir -> ParamName Organisation -> ParamName Flow -> IO (Path Abs File)
parseCachePath dir org flow =
  (\o f -> dir </> flowsRelPath </> o </> f) <$> parseRelDir (getParamName org) <*> parseRelFile (getParamName flow)

saveRows :: Path Abs File -> [Row] -> IO ()
saveRows filepath rows = do
  let bytes = runPut (mapM_ (put . binaryTag') rows)
  createDirectoryIfMissing True (toFilePath (parent filepath))
  LBS.appendFile (toFilePath filepath) bytes

readRows :: Path Abs File -> IO ([Row], Bool)
readRows filepath  = do
  contents <- LBS.readFile (toFilePath filepath)
  let g = (,) <$> many get <*>Data.Binary.Get.isEmpty :: Get ([BinaryTagged (SemanticVersion Row) Row], Bool)
  return $ first (fmap binaryUntag') $ runGet g contents

grepRow :: Text -> [Row] -> IO (Maybe Row)
grepRow needle rows = go rows
  where go []           = return Nothing
        go [row]         = p row >> return (Just row)
        go (row:rows')  = p row >> go rows'

        p :: Row -> IO ()
        p row = when (needle `T.isInfixOf` (rowText row))
                     (print row)

main' :: Path Abs Dir -> Bool -> Opts -> IO ()
main' settingsDirectory writeToken opts = do
  let token = optsToken opts
  let org = optsOrganisation opts
  let flow = optsFlow opts

  -- Save auth token
  when writeToken $ writeAuthToken settingsDirectory token

  -- Cache file
  cachePath <- parseCachePath settingsDirectory org flow

  -- Read from cache
  (rows, allRead) <- readRows cachePath `catch` onIOError ([], True)
  lastRow <- grepRow (T.pack $ optsNeedle opts) rows
  when (not allRead) $ Prelude.putStrLn "Error: corrupted cache file, removing..." >> removeFile (toFilePath cachePath)

  -- Read from API
  when (not $ optsOffline opts) $ do
    mgr <- newManager tlsManagerSettings
    loop mgr token org flow cachePath (T.pack $ optsNeedle opts) lastRow

loop :: Manager -> AuthToken -> ParamName Organisation -> ParamName Flow -> Path Abs File -> Text -> Maybe Row -> IO ()
loop mgr token org flow cachePath needle = go
  where go lastRow = do req <- untag <$> messagesRequest org flow (baseMessageOptions $ rowMessageId <$> lastRow)
                        let req' = authenticateRequest token req
                        print lastRow
                        res <- httpLbs req' mgr
                        rows <- mapMaybe messageToRow <$> throwDecode (responseBody res) :: IO [Row]
                        saveRows cachePath rows
                        lastRow' <- grepRow needle rows
                        -- Loop only if we got something
                        when (isJust lastRow') $ go lastRow'

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

onIOError :: a -> IOError -> IO a
onIOError x _ = return x
