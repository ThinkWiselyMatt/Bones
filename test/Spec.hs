

import Test.Hspec
import qualified WebServiceSpecs (spec)
--import qualified QuickCheckSpecs (spec)

main :: IO ()
main = hspec $ do
  describe "WebService Specs" WebServiceSpecs.spec
  --describe "QuickCheck Specs" QuickCheckSpecs.spec