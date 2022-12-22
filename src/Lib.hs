{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( fetchCoAuthorInfo,
      parseCoAuthorXml,
      CoAuthorInfo(..),
      PersonInfo(..)
    ) where

import qualified Data.ByteString.Char8 as B8
import Network.HTTP.Simple

data PersonInfo = PersonInfo {
  name :: String,
  urlpt :: String,
  pid :: String
} deriving (Eq, Show)

data CoAuthorInfo = CoAuthorInfo {
  person :: PersonInfo,
  coAuthors :: [PersonInfo]
} deriving (Eq, Show)

fetchCoAuthorInfo :: IO ()
fetchCoAuthorInfo = do
  res <- httpBS "https://dblp.org/rec/pers/m/McCallum:Andrew/xc"
  B8.putStrLn $ getResponseBody res
  pure ()

parseCoAuthorXml :: String -> CoAuthorInfo
parseCoAuthorXml txt = undefined
