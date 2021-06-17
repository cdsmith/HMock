import Classes (classTests)
import Core (coreTests)
import Demo (demoSpec)
import qualified DocTests.All
import Extras (multiplicityTests, predicateTests)
import qualified Test.DocTest.Driver as DocTest
import Test.Hspec (hspec)
import TH
main :: IO ()
main = do
  hspec $ do
    multiplicityTests
    predicateTests
    coreTests
    classTests
    thUtilSpec
    demoSpec
  DocTest.run DocTests.All.main
