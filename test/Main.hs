import Classes (classTests)
import Core (coreTests)
import Extras (multiplicityTests, predicateTests)
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  multiplicityTests
  predicateTests
  coreTests
  classTests
