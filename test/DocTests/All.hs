-- Do not edit! Automatically created with doctest-extract.
module DocTests.All where

import qualified DocTests.Test.HMock.Internal.FlowMatcher
import qualified DocTests.Test.HMock.Predicates
import qualified DocTests.Test.HMock.Multiplicity

import qualified Test.DocTest.Driver as DocTest

main :: DocTest.T ()
main = do
    DocTests.Test.HMock.Internal.FlowMatcher.test
    DocTests.Test.HMock.Predicates.test
    DocTests.Test.HMock.Multiplicity.test
