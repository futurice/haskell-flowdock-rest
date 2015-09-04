{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.Caching
import Control.Monad.Catch
import Control.Monad.HTTP
import Control.Monad.IO.Class
import Data.Aeson
import Data.ByteString.Char8 as BSChar8
import Data.ByteString.Lazy as LBS hiding (map, foldr)
import Data.CaseInsensitive as CI
import Data.Char
import Data.List as L
import Data.Maybe
import Data.Monoid
import Data.Function
import Data.Tagged
import Data.Text as T
import Data.Text.Encoding as E
import Github.Auth
import Data.HashMap.Strict as SHM
import Github.Data
import Github.Repos hiding (contributors)
import Network.HTTP.Client hiding (httpLbs)
import Network.HTTP.Client.TLS
import Network.HTTP.Types
import Text.PrettyPrint.ANSI.Leijen hiding ((<>), (<$>))
import Text.Regex.Applicative.Common
import Text.Regex.Applicative.Text as RE
import Text.Regex (mkRegex, matchRegex)

import Chat.Flowdock.REST

stringTrim :: String -> String
stringTrim = r . ltrim . r . ltrim
  where ltrim = Prelude.dropWhile isSpace
        r = Prelude.reverse

readAuthToken :: IO AuthToken
readAuthToken = do
  contents <- Prelude.readFile ".flowdock-rest-api-token"
  return $ AuthToken $ stringTrim contents

throwDecode :: (MonadThrow m, FromJSON a) => LBS.ByteString -> m a
throwDecode bs = case eitherDecode bs of
  Right x   -> return x
  Left err  -> error $ "throwDecode: " <> err

mailMessageOpts :: MessageOptions
mailMessageOpts =
  defMessageOptions & msgOptEvent .~ Just EventMail
                    & msgOptLimit .~ Just 100

fetchMessages :: AuthToken -> Manager -> MessageOptions -> IO [Message]
fetchMessages token mgr msgOpts = do
  req <- messagesRequest (mkParamName "futurice") (mkParamName "open-source") msgOpts
  -- print req
  let req' = authenticateRequest token $ unTagged req
  res <- flip runHttpT mgr . flip runCachingT (responseDirectoryCache ".cache") $ httpLbs req'
  throwDecode (responseBody res)

fetchMessagesLoop :: AuthToken -> Manager -> Maybe MessageId -> IO [Message]
fetchMessagesLoop token mgr = go
  where
    go :: Maybe MessageId -> IO [Message]
    go mMsgId = do let msgOpts = mailMessageOpts & msgOptUntilId .~ mMsgId
                   msgs <- fetchMessages token mgr msgOpts
                   case msgs of
                     []    -> return []
                     (x:_) -> (<> msgs) <$> go (Just $ x ^. msgId)

digittt :: RE' Char
digittt = psym isDigit

floatRe :: RE' Float
floatRe = f <$> some digittt <*> optional (dot *> some digittt)
  where dot = (sym ',' <|> sym '.')
        f xs ys = read (xs <> "." <> fromMaybe "0" ys)

hoursRe :: RE' Float
hoursRe = floatRe <* many (psym isSpace) <* (RE.string "h" <|> RE.string "H")

githubRe :: RE' (String, String)
githubRe = (,) <$ RE.string "github.com/" <*> some (RE.psym p) <* RE.sym '/' <*> some (RE.psym p)
 where p c = isAlphaNum c || c `L.elem` "-_."

sndOf3 :: (a, b, c) -> b
sndOf3 (_, b, _) = b

extractContribution :: Mail -> Contribution
extractContribution mail = Contribution (fromMaybe 0 hours) (mail ^. mailFrom) subject github
  where
    subject = mail ^. mailSubject
    content = mail ^. mailContent
    github = sndOf3 <$> (findFirstInfix githubRe content)
    hours = sndOf3 <$> (findFirstInfix hoursRe subject  <|> findFirstInfix hoursRe content)

expandContribution :: Manager -> (String, String) -> IO Value
expandContribution mgr (u, r) = flip runHttpT mgr . flip runCachingT (responseDirectoryCache ".cache") $ languagesForIO u r

contributors :: [Mail] -> [Text]
contributors msgs = L.sort . nub $ msgs ^.. traverse . mailFrom . traverse . to mailName

mailName :: MailAddress -> Text
mailName a = fromMaybe (a ^. mailAddress) (a ^. mailAddressName)

data Contribution = Contribution
  { contrHours :: Float
  , contrAddress :: [MailAddress]
  , contrSubject :: Text
  , contrGithub :: Maybe (String, String)
  }
  deriving (Show)

instance Pretty Contribution where
  pretty (Contribution hours address subj github) = pretty hours <+> pretty address <+> pretty github <+> pretty subj

plusU :: Value -> Value -> Value
plusU (Object a) (Object b) = Object $ SHM.unionWith plus a b
plusU (Object a) _          = Object a
plusU _ (Object b)          = Object b
plusU _ _                   = Object $ SHM.empty

plus :: Value -> Value -> Value
plus (Number n) (Number m) = Number (n + m)
plus (Number n) _          = Number n
plus _          (Number m) = Number m
plus x          _          = Number 0

stats :: Manager -> [Message] -> IO ()
stats mgr msgs = do
  let mails = msgs ^.. traverse . msgContent . _MTMail
  let contrs = fmap extractContribution mails
  langs <- traverse (expandContribution mgr) (nub $ mapMaybe contrGithub contrs)
  let totalLangs = L.foldl' plusU (Object SHM.empty) langs
  mapM_ (putDoc . (<> linebreak) . pretty) contrs
  putDoc $ pretty (contributors mails) <> linebreak
  putDoc $ pretty $ text "Contributions in total" <+> pretty (Prelude.length msgs) <> linebreak
  putDoc $ pretty $ text "Unique contributors" <+> pretty (Prelude.length $ contributors mails) <> linebreak
  putDoc $ pretty $ text "Hours reported" <+> pretty (sum $ fmap contrHours contrs) <> linebreak
  printT totalLangs

compareValue (Number a) (Number b) = compare a b
compareValue _ _ = EQ

printT (Object s) = mapM_ print $ L.sortBy (compareValue `on` snd) $ SHM.toList s

main :: IO ()
main = do
  token <- liftIO readAuthToken
  mgr <- newManager tlsManagerSettings
  msgs <- fetchMessagesLoop token mgr Nothing
  stats mgr msgs

languagesForIO :: (MonadHTTP m, MonadThrow m) => String -> String -> m Value
languagesForIO userName reqRepoName = getSingle ["repos", userName, reqRepoName, "languages"] Nothing

reqBuilder :: MonadThrow m => Maybe GithubAuth -> URL -> m Request
reqBuilder auth url = do
  req <- parseUrl url
  let req' = applyGithubAuth auth req
  let userAgentHeader = ("User-Agent", "https://github.com/futurice/fum2github")
  let req'' = req'  { requestHeaders = userAgentHeader : requestHeaders req' }
  return req''

resParser :: (MonadThrow m, FromJSON a) => Response LBS.ByteString -> m (Maybe URL, a)
resParser res = do
  let body = responseBody res
      hdrs = responseHeaders res
  value <- throwDecode body
  return (nextUrl hdrs, value)

getMulti :: (MonadThrow m, MonadHTTP m, FromJSON a)
          => [URLPart]
          -> Maybe GithubAuth
          -> m [a]
getMulti parts auth =
  Prelude.concat `liftM` getPaginatedResponses (reqBuilder auth) resParser url
  where url = buildUrl parts

-- | Fetch paginated resourses.
getPaginatedResponses :: (MonadHTTP m)
                      => (a -> m Request)                          -- ^ Request builder
                      -> (Response LBS.ByteString -> m (Maybe a, b)) -- ^ Response parser into next request data, and parsed response value
                      -> a                                         -- ^ Initial request seed
                      -> m [b]
getPaginatedResponses builder parser = go
    where go s = do req <- builder s
                    res <- httpLbs req
                    (next, v) <- parser res
                    case next of
                        Just next' -> (v :) `liftM` go next'  -- TODO: use fmap with base >=4.8
                        Nothing    -> return [v]

-- | Fancier version of 'httpLbs'.
getSingleResponse :: (MonadHTTP m)
                  => (a -> m Request)                          -- ^ Request builder
                  -> (Response LBS.ByteString -> m (Maybe a, b)) -- ^ Response parser into next request data, and parsed response value
                  -> a                                         -- ^ Initial request seed
                  -> m b
getSingleResponse builder parser s = do
  req <- builder s
  res <- httpLbs req
  snd <$> parser res

-- | Short, one line summary of response. Useful for logging
requestUrl :: Request -> String
requestUrl req = s ++ h ++ p ++ q
  where s = if secure req then "https://" else "http://"
        h = BSChar8.unpack $ host req
        p = BSChar8.unpack $ path req
        q = BSChar8.unpack (queryString req)

buildUrl :: [URLPart] -> URL
buildUrl parts = "https://api.github.com/" ++ L.intercalate "/" parts

nextUrl :: ResponseHeaders -> Maybe URL
-- TODO rewrite using regex-applicative
nextUrl headers =
    L.foldr foldFunc Nothing matches
    where
      linkHeaders = L.filter ((== "Link") . fst) headers
      strVals = L.map (T.unpack . E.decodeUtf8 . snd) linkHeaders
      re = mkRegex "<([^>]+)>; rel=\"next\""
      matches = L.map (matchRegex re) strVals

      foldFunc :: Maybe [String] -> Maybe URL -> Maybe URL
      foldFunc (Just [x]) _ = Just x
      foldFunc _ x = x

type URL = String
type URLPart = String

applyGithubAuth :: Maybe GithubAuth -> Request -> Request
applyGithubAuth Nothing req = req
applyGithubAuth (Just (GithubBasicAuth user pass)) req = applyBasicAuth user pass req
applyGithubAuth (Just (GithubOAuth token)) req =
  let header = (CI.mk (BSChar8.pack "Authorization"),  BSChar8.pack ("token " ++ token))
  in req { requestHeaders = header : requestHeaders req }


getSingle :: (MonadThrow m, MonadHTTP m, FromJSON a)
          => [URLPart]
          -> Maybe GithubAuth
          -> m a
getSingle parts auth =
  getSingleResponse (reqBuilder auth) resParser url
  where url = buildUrl parts
