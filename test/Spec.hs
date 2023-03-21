import Test.Hspec

import DblpCoAuthorApiSpec
import DblpOrcidApiSpec

main :: IO ()
main = hspec $ do
  describe "DblpCoAuthorApi" dblpCoAuthorApiSpec
  describe "DblpOrcidApiSpec" dblpOrcidApiSpec
