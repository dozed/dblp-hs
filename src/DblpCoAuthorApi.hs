{-# LANGUAGE OverloadedStrings #-}

module DblpCoAuthorApi
    ( fetchCoAuthorInfo,
      parseCoAuthorXml,
      parseCoAuthorXml',
      mkCoAuthorXmlApiUrl,
      DblpError(..),
      CoAuthorInfo(..),
      PersonInfo(..),
      UrlPt(..)
    ) where

import qualified Data.ByteString.Lazy as LBS
import Data.Either.Combinators (maybeToRight)
import qualified Data.Map as M
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Client (parseRequest)
import Network.HTTP.Simple (httpLBS, getResponseBody)
import Text.XML
import Text.XML.Cursor

import Types

data PersonInfo = PersonInfo {
  name :: Text,
  urlpt :: Text,
  pid :: Text
} deriving (Eq, Show)

data CoAuthorInfo = CoAuthorInfo {
  author :: PersonInfo,
  coAuthors :: [PersonInfo]
} deriving (Eq, Show)

newtype UrlPt = UrlPt { unUrlPt :: String }

mkCoAuthorXmlApiUrl :: UrlPt -> String
mkCoAuthorXmlApiUrl (UrlPt urlpt) = "https://dblp.org/rec/pers/" <> urlpt <> "/xc"

fetchCoAuthorInfo :: UrlPt -> IO (Either DblpError CoAuthorInfo)
fetchCoAuthorInfo urlpt = do
  let url = mkCoAuthorXmlApiUrl urlpt
  req <- parseRequest url
  res <- httpLBS req
  let bs = getResponseBody res
  return $ parseCoAuthorXml bs

-- Parse CoAuthorInfo using Text.XML
getElementFromNode :: Node -> Maybe Element
getElementFromNode (NodeElement e) = Just e
getElementFromNode _ = Nothing

getContentFromNode :: Node -> Maybe Text
getContentFromNode (NodeContent txt) = Just txt
getContentFromNode _ = Nothing

parseCoAuthorXml :: LBS.ByteString -> Either DblpError CoAuthorInfo
parseCoAuthorXml txt =
  case parseLBS def txt of
    Left e -> Left (ParseError (show e))
    Right (Document prologue root epilogue) ->
      let coAuthorInfo = parseCoAuthorInfo root
      in maybeToRight (ParseError "could not parse CoAuthorInfo") coAuthorInfo

-- TODO use proper validation
parseCoAuthorInfo :: Element -> Maybe CoAuthorInfo
parseCoAuthorInfo root@(Element _ _ children) = do
  author <- parseAuthorPersonInfo root
  coAuthors <- sequence . filter isJust . map (getElementFromNode >=> parseCoAuthorPersonInfo) $ children
  let coAuthorInfo = CoAuthorInfo {
                       author = author,
                       coAuthors = coAuthors
                     }
  return coAuthorInfo

parseAuthorPersonInfo :: Element -> Maybe PersonInfo
parseAuthorPersonInfo (Element _ attrs _) = do
  name <- M.lookup "author" attrs
  urlpt <- M.lookup "urlpt" attrs
  pid <- M.lookup "pid" attrs
  return PersonInfo { name = name, urlpt = urlpt, pid = pid }

parseCoAuthorPersonInfo :: Element -> Maybe PersonInfo
parseCoAuthorPersonInfo (Element _ _ []) = Nothing
parseCoAuthorPersonInfo (Element _ attrs (child:_)) = do
  name <- getContentFromNode child
  urlpt <- M.lookup "urlpt" attrs
  pid <- M.lookup "pid" attrs
  return PersonInfo { name = name, urlpt = urlpt, pid = pid }

-- Parse CoAuthorInfo using Text.XML.Cursor
parseCoAuthorXml' :: LBS.ByteString -> Either DblpError CoAuthorInfo
parseCoAuthorXml' txt =
  case parseLBS def txt of
    Left e -> Left (ParseError (show e))
    Right doc ->
      let cursor = fromDocument doc
          coAuthorInfo = parseCoAuthorInfo' cursor
      in Right coAuthorInfo

parseCoAuthorInfo' :: Cursor -> CoAuthorInfo
parseCoAuthorInfo' cursor =
  let author = parseAuthorPersonInfo' cursor
      coAuthors = map parseCoAuthorPersonInfo' $ cursor $/ element "author"
      coAuthorInfo = CoAuthorInfo {
        author = author,
        coAuthors = coAuthors
      }
  in coAuthorInfo

parseAuthorPersonInfo' :: Cursor -> PersonInfo
parseAuthorPersonInfo' cursor =
  let name = T.concat $ attribute "author" cursor
      urlpt = T.concat $ attribute "urlpt" cursor
      pid = T.concat $ attribute "pid" cursor
      authorInfo = PersonInfo { name = name, urlpt = urlpt, pid = pid }
  in authorInfo

parseCoAuthorPersonInfo' :: Cursor -> PersonInfo
parseCoAuthorPersonInfo' cursor =
  let name = T.concat $ cursor $// content
      urlpt = T.concat $ attribute "urlpt" cursor
      pid = T.concat $ attribute "pid" cursor
      authorInfo = PersonInfo { name = name, urlpt = urlpt, pid = pid }
  in authorInfo
