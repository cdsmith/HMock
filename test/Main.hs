import Classes (classTests)
import Core (coreTests)
import Extras (multiplicityTests, predicateTests)
import Test.Hspec (hspec)
import Test.DocTest

runDoctest :: IO ()
runDoctest = doctest [
  "-isrc",
  "src/Test/HMock/Internal/Predicates.hs"
  ]

main :: IO ()
main = do
  runDoctest
  hspec $ do
    multiplicityTests
    predicateTests
    coreTests
    classTests
