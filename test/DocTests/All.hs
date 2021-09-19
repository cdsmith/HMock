-- Do not edit! Automatically created with doctest-extract.
module DocTests.All where

import qualified DocTests.Test.HMock.Multiplicity

import qualified Test.DocTest.Driver as DocTest

main :: DocTest.T ()
main = do
    DocTests.Test.HMock.Multiplicity.test
