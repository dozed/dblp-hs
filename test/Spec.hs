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
</coauthor>
|]

coAuthorInfo :: CoAuthorInfo
coAuthorInfo = CoAuthorInfo {
  person = PersonInfo { name = "", urlpt = "", pid = "" },
  coAuthors = [
      PersonInfo { name = "", urlpt = "", pid = "" },
      PersonInfo { name = "", urlpt = "", pid = "" }
    ]
}

main :: IO ()
main = hspec $ do
  describe "parseCoAuthorXml" $ do
    it "should parse co-author xml" $ do
      parseCoAuthorXml coAuthorInfoXmlText `shouldBe` coAuthorInfo

