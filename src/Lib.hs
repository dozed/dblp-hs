{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( fetchCoAuthorInfo,
      parseCoAuthorXml,
      stringToLBS,
      lbsToString,
      mkCoAuthorXmlApiUrl,
      CoAuthorInfo(..),
      PersonInfo(..)
    ) where

import Control.Exception (Exception)
import Control.Monad ((>=>))
import qualified Data.ByteString.Lazy as LBS
import Data.Either.Combinators (maybeToRight)
import qualified Data.Map as M
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Network.HTTP.Simple
import Text.XML

data DblpException = ParseError String
  deriving (Eq, Show)

instance Exception DblpException

data PersonInfo = PersonInfo {
  name :: Text,
  urlpt :: Text,
  pid :: Text
} deriving (Eq, Show)

data CoAuthorInfo = CoAuthorInfo {
  person :: PersonInfo,
  coAuthors :: [PersonInfo]
} deriving (Eq, Show)

stringToLBS :: String -> LBS.ByteString
stringToLBS = TL.encodeUtf8 . TL.pack

lbsToString :: LBS.ByteString -> String
lbsToString = TL.unpack . TL.decodeUtf8

getElementFromNode :: Node -> Maybe Element
getElementFromNode (NodeElement e) = Just e
getElementFromNode _ = Nothing

getContentFromNode :: Node -> Maybe Text
getContentFromNode (NodeContent txt) = Just txt
getContentFromNode _ = Nothing

mkCoAuthorXmlApiUrl :: String -> String
mkCoAuthorXmlApiUrl urlpt = "https://dblp.org/rec/pers/" <> urlpt <> "/xc"

fetchCoAuthorInfo :: String -> IO (Either DblpException CoAuthorInfo)
fetchCoAuthorInfo urlpt = do
  let url = mkCoAuthorXmlApiUrl urlpt
  req <- parseRequest url
  res <- httpLBS req
  let bs = getResponseBody res
  return $ parseCoAuthorXml bs

parseCoAuthorXml :: LBS.ByteString -> Either DblpException CoAuthorInfo
parseCoAuthorXml txt =
  case parseLBS def txt of
    Left e -> Left (ParseError (show e))
    Right (Document prologue root epilogue) ->
      let cai = parseCoAuthorInfo root
      in maybeToRight (ParseError "could not parse CoAuthorInfo") cai

parseCoAuthorInfo :: Element -> Maybe CoAuthorInfo
parseCoAuthorInfo root@(Element _ _ children) = do
  author <- parseAuthorPersonInfo root
  coAuthors <- sequence . filter isJust . map (getElementFromNode >=> parseCoAuthorPersonInfo) $ children
  let cai = CoAuthorInfo {
              person = author,
              coAuthors = coAuthors
            }
  return cai

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
