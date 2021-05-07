import Classes (classTests)
import Core (coreTests)
import Extras (cardinalityTests, predicateTests)
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  cardinalityTests
  predicateTests
  coreTests
  classTests
