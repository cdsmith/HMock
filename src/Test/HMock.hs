-- |
--
-- HMock is a mock framework for testing effectful code.  You define the
-- actions you expect and their results, run the code under test, and HMock
-- verifies that what happened matched your expectations.  For an introduction
-- to the idea of mocks, see
-- <https://martinfowler.com/articles/mocksArentStubs.html Mocks Aren't Stubs>,
-- by Martin Fowler.
--
-- HMock can mock two kinds of interface:
--
-- ["Test.HMock.Classic"] Mocks for @mtl@-style type classes.  Tests run in
--   the t'MockT' monad transformer, and a class is mocked by generating an
--   instance of it for @MockT@.  This module is a re-export of it.
--
-- ["Test.HMock.Bluefin"] Mocks for
--   [Bluefin](https://hackage.haskell.org/package/bluefin) capabilities.
--   Tests run in @Eff@, a capability is mocked by building a record whose
--   operations delegate to the mock, and expectations belong to a particular
--   mocked instance rather than to a type.
--
module Test.HMock (module Test.HMock.Classic) where

import Test.HMock.Classic
