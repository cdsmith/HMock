-- | The test suite, in three parts:
--
-- * "Common": the parts of HMock every front end shares.
-- * "Classic": the mtl-style front end, "Test.HMock.Classic".
-- * "Bluefin": the Bluefin front end, "Test.HMock.Bluefin".
--
-- "Support" holds what the parts share, chiefly a mock of Template Haskell's
-- 'Language.Haskell.TH.Syntax.Quasi' class for driving the splices under test.
module Main (main) where

import qualified Bluefin.Core
import qualified Bluefin.Demo
import qualified Bluefin.Manual
import qualified Bluefin.Readme
import qualified Bluefin.Splice
import qualified Bluefin.TH
import qualified Classic.Classes
import qualified Classic.Core
import qualified Classic.Demo
import qualified Common.DocTests.All
import qualified Common.ExpectSet
import qualified Common.Multiplicity
import qualified Common.THUtil
import qualified Test.DocTest.Driver as DocTest
import Test.Hspec (hspec)

main :: IO ()
main = do
  hspec $ do
    Common.Multiplicity.multiplicityTests
    Common.ExpectSet.expectSetSpec
    Common.THUtil.thUtilSpec

    Classic.Core.coreTests
    Classic.Classes.classTests
    Classic.Demo.demoSpec

    Bluefin.Manual.bluefinManualSpec
    Bluefin.Core.bluefinCoreSpec
    Bluefin.TH.bluefinTHSpec
    Bluefin.Readme.bluefinReadmeSpec
    Bluefin.Splice.bluefinSpliceSpec
    Bluefin.Demo.bluefinDemoSpec
  DocTest.run Common.DocTests.All.main
