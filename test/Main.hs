import Classes (classTests)
import Core (coreTests)
import qualified DocTests.All
import Extras (multiplicityTests, predicateTests)
import qualified Test.DocTest.Driver as DocTest
import Test.Hspec (hspec)

main :: IO ()
main = do
  hspec $ do
    multiplicityTests
    predicateTests
    coreTests
    classTests
  DocTest.run DocTests.All.main
