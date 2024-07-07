

import Test.Hspec
import qualified WebServiceSpecs (spec)

main :: IO ()
main = hspec WebServiceSpecs.spec