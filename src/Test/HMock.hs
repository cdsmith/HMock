{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
--
-- This module provides a monad transformer, 'MockT', which can be used to test
-- with mocks of Haskell @mtl@-style type classes.  To use a mock, you define
-- the expected actions and their results, and then run the code you are
-- testing.  The framework verifies that the behavior of the code matched your
-- expectations.
--
-- For an introduction to the idea of mocks, see
-- <https://martinfowler.com/articles/mocksArentStubs.html Mocks Aren't Stubs>,
-- by Martin Fowler.
--
-- WARNING: Hmock's API is likely to change soon.  Please ensure you use an
-- upper bound on the version number.  The current API works fine for mocking
-- with MTL-style classes.  I want HMock to also work with effect systems,
-- servant, haxl, and more.  To accomplish this, I'll need to make breaking
-- changes to the API.
--
-- Suppose you have a @MonadFilesystem@ typeclass, which is instantiated by
-- monads that implement filesystem operations:
--
-- @
-- class 'Monad' m => MonadFilesystem m where
--   readFile :: 'FilePath' -> m 'String'
--   writeFile :: 'FilePath' -> 'String' -> m ()
-- @
--
-- You can use HMock to test code using @MonadFilesystem@ like this:
--
-- @
-- copyFile :: MonadFilesystem m => 'FilePath' -> 'FilePath' -> m ()
-- copyFile a b = readFile a >>= writeFile b
--
-- 'Test.HMock.TH.makeMockable' ''MonadFilesystem
--
-- spec = describe "copyFile" '$'
--   it "reads a file and writes its contents to another file" '$'
--     'runMockT' '$' do
--       'expect' '$' ReadFile "foo.txt" '|->' "contents"
--       'expect' '$' WriteFile "bar.txt" "contents" '|->' ()
--       copyFile "foo.txt" "bar.txt"
-- @
--
-- The Template Haskell splice, 'Test.HMock.TH.makeMockable', generates the
-- boilerplate needed to use @MonadFilesystem@ with HMock.  You then use
-- 'runMockT' to begin a test with mocks, 'expect' to set up your expected
-- actions and responses, and finally execute your code.
module Test.HMock
  ( -- * Running mocks
    MockT,
    runMockT,
    withMockT,
    describeExpectations,
    verifyExpectations,
    setAmbiguityCheck,

    -- * Rules for calls and responses
    module Test.HMock.Rule,

    -- * Combinators for building test plans
    module Test.HMock.ExpectContext,

    -- * Initializing mockable classes
    MockSetupContext,
    MockSetupT,
    byDefault,
    setDefault,

    -- * Predicates
    module Test.HMock.Predicates,

    -- * Multiplicity
    module Test.HMock.Multiplicity,

    -- * Implementing mocks
    module Test.HMock.Mockable,
    mockMethod,
    mockDefaultlessMethod,
  )
where

import Test.HMock.ExpectContext
import Test.HMock.Internal.MockT
import Test.HMock.Mockable
import Test.HMock.Multiplicity
import Test.HMock.Predicates
import Test.HMock.Rule
