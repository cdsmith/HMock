-- Do not edit! Automatically created with doctest-extract.
module Common.DocTests.All where

import qualified Common.DocTests.Test.HMock.Multiplicity

import qualified Test.DocTest.Driver as DocTest

main :: DocTest.T ()
main = do
    Common.DocTests.Test.HMock.Multiplicity.test
