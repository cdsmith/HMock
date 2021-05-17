-- Do not edit! Automatically created with doctest-extract.
module DocTests.All where

import qualified DocTests.Test.HMock.Internal.Predicates
import qualified DocTests.Test.HMock.Internal.Multiplicity

import qualified Test.DocTest.Driver as DocTest

main :: DocTest.T ()
main = do
    DocTests.Test.HMock.Internal.Predicates.test
    DocTests.Test.HMock.Internal.Multiplicity.test
