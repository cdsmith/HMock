import Classes (classTests)
import Core (coreTests)
import Demo (demoSpec)
import qualified DocTests.All
import ExpectSet (expectSetSpec)
import Multiplicity (multiplicityTests)
import TH (thUtilSpec)
import qualified Test.DocTest.Driver as DocTest
import Test.Hspec (hspec)

main :: IO ()
main = do
  hspec $ do
    multiplicityTests
    expectSetSpec
    coreTests
    classTests
    thUtilSpec
    demoSpec
  DocTest.run DocTests.All.main
