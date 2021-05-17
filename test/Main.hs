import Classes (classTests)
import Core (coreTests)
import Extras (multiplicityTests, predicateTests)
import Test.DocTest (doctest)
import Test.Hspec (hspec)

runDoctest :: IO ()
runDoctest =
  doctest
    [ "-isrc",
      "src/Test/HMock/Internal/Predicates.hs",
      "src/Test/HMock/Internal/Multiplicity.hs"
    ]

main :: IO ()
main = do
  runDoctest
  hspec $ do
    multiplicityTests
    predicateTests
    coreTests
    classTests
