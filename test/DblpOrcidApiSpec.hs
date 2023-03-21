module DblpOrcidApiSpec (dblpOrcidApiSpec) where

import Control.Monad.IO.Class (liftIO)
import Data.Either (isLeft)

import Test.Hspec

import DblpOrcidApi
import Types

dblpOrcidApiSpec :: Spec
dblpOrcidApiSpec = do

  describe "mkOrcidToPersonIdResolutionApiUrl" $ do
    it "should create correct URL" $ do
      mkOrcidToPersonIdResolutionApiUrl (Orcid "0000-0001-8675-6631") `shouldBe` "https://dblp.org/orcid/0000-0001-8675-6631"

  describe "parsePersonIdFromUrl" $ do
    it "should parse a DblpPersonId from a person url" $ do
      parsePersonIdFromUrl "https://dblp.org/pid/81/1984.html" `shouldBe` Just (DblpPersonId "81/1984")

    it "should not parse a DblpPersonId from a non-person url" $ do
      parsePersonIdFromUrl "https://dblp.org/foo/81/1984.html" `shouldBe` Nothing

  describe "resolveOrcidToPersonId" $ do
    it "should return the DblpPersonId" $ do
      let orcid = Orcid "0000-0001-8675-6631"
          expectedPersonId = DblpPersonId "81/1984"

      personId <- liftIO $ resolveOrcidToPersonId orcid

      personId `shouldBe` Right expectedPersonId

    it "should return error for wrong Orcid" $ do
      let orcid = Orcid "foo"

      personId <- liftIO $ resolveOrcidToPersonId orcid

      personId `shouldSatisfy` isLeft
