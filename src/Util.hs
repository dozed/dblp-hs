module Util
    ( stringToLBS,
      lbsToString,
    ) where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

stringToLBS :: String -> LBS.ByteString
stringToLBS = TL.encodeUtf8 . TL.pack

lbsToString :: LBS.ByteString -> String
lbsToString = TL.unpack . TL.decodeUtf8
