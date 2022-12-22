{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import Text.RawString.QQ

import Test.Hspec

import Lib

coAuthorInfoXmlText :: String
coAuthorInfoXmlText = [r|
<?xml version="1.0" encoding="US-ASCII"?>
<coauthors author="Andrew McCallum" urlpt="m/McCallum:Andrew" pid="m/AndrewMcCallum" n="410">
<author urlpt="a/Abadi:Mart=iacute=n" pid="a/MartinAbadi" count="2">Mart&#237;n Abadi</author>
<author urlpt="a/Abdelaziz:Ibrahim" pid="153/1958" count="1">Ibrahim Abdelaziz</author>
</coauthors>
|]

coAuthorInfo :: CoAuthorInfo
coAuthorInfo = CoAuthorInfo {
  person = PersonInfo { name = "Andrew McCallum", urlpt = "m/McCallum:Andrew", pid = "m/AndrewMcCallum" },
  coAuthors = [
      PersonInfo { name = "Martín Abadi", urlpt = "a/Abadi:Mart=iacute=n", pid = "a/MartinAbadi" },
      PersonInfo { name = "Ibrahim Abdelaziz", urlpt = "a/Abdelaziz:Ibrahim", pid = "153/1958" }
    ]
}

main :: IO ()
main = hspec $ do
  describe "mkCoAuthorXmlApiUrl" $ do
    it "should create a valid co-author xml api url" $ do
      mkCoAuthorXmlApiUrl "m/McCallum:Andrew" `shouldBe` "https://dblp.org/rec/pers/m/McCallum:Andrew/xc"
  
  describe "parseCoAuthorXml" $ do
    it "should parse co-author xml" $ do
      parseCoAuthorXml (stringToLBS coAuthorInfoXmlText) `shouldBe` Right coAuthorInfo

