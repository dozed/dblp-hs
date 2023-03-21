{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module DblpOrcidApi where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import Network.HTTP.Types.Header (hLocation, hUserAgent, hAccept)
import Network.HTTP.Client (Request(..), Response(..), parseRequest)
import Network.HTTP.Simple (httpLBS, getResponseStatusCode, getResponseHeader, setRequestHeaders)
import Safe (headMay)
import qualified Data.Text.Encoding as T
import Text.Regex.PCRE.Heavy (re, scan)

import Types (DblpError(..), DblpPersonId(..), Orcid(..))

mkOrcidToPersonIdResolutionApiUrl :: Orcid -> String
mkOrcidToPersonIdResolutionApiUrl (Orcid orcidStr) = "https://dblp.org/orcid/" <> orcidStr

parsePersonIdFromUrl :: String -> Maybe DblpPersonId
parsePersonIdFromUrl txt = do
  (_, dblpPersonIdTxt:_) <- headMay $ scan [re|https://dblp.org/pid/(.+)\.html|] txt
  Just (DblpPersonId dblpPersonIdTxt)

parsePersonIdFromResponse :: Orcid -> Response BSL.ByteString -> Either DblpError DblpPersonId
parsePersonIdFromResponse orcid res = do
  _ <- case getResponseStatusCode res of
    302 -> Right ()
    404 -> Left $ OrcidNotFound $ "Could not resolve ORCID: " <> show orcid
    statusCode -> Left $ ApiError $ "Unexpected response status code: " <> show statusCode <> " for ORCID: " <> show orcid

  location <- case getResponseHeader hLocation res of
    [] -> Left $ ApiError $ "No Location header for ORCID: " <> show orcid
    location:_ -> Right location

  case parsePersonIdFromUrl (T.unpack . T.decodeUtf8 $ location) of
    Nothing -> Left $ ApiError $ "Could not parse Location: " <> show location <> " for ORCID: " <> show orcid
    Just personId -> Right personId

resolveOrcidToPersonId :: Orcid -> IO (Either DblpError DblpPersonId)
resolveOrcidToPersonId orcid = do
  let url = mkOrcidToPersonIdResolutionApiUrl orcid
  req <- parseRequest url
  let req' = setRequestHeaders [(hUserAgent, "curl/7.85.0"), (hAccept, "*/*")] req
  let req'' = req' { redirectCount = 0 }
  res <- httpLBS req''
  let personId = parsePersonIdFromResponse orcid res
  return personId
