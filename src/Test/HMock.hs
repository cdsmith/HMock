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
--       'expect' '$' readFile_ "foo.txt" '|->' "contents"
--       'expect' '$' writeFile_ "bar.txt" "contents" '|->' ()
--       copyFile "foo.txt" "bar.txt"
-- @
--
-- The Template Haskell splice, 'Test.HMock.TH.makeMockable', generates the
-- boilerplate needed to use @MonadFilesystem@ with HMock.  You then use
-- 'runMockT' to begin a test with mocks, 'expect' to set up your expected
-- actions and responses, and finally execute your code.
module Test.HMock
  ( -- * Core interface
    MockT,
    runMockT,
    Rule ((:->)),
    (|->),
    Expectable,
    expect,
    expectN,
    expectAny,
    whenever,
    inSequence,
    inAnyOrder,

    -- * Predicates
    Predicate (..),
    eq,
    neq,
    gt,
    geq,
    lt,
    leq,
    anything,
    andP,
    orP,
    notP,
    startsWith,
    endsWith,
    hasSubstr,
    size,
    elems,
    allElems,
    anyElem,
    suchThat,
    match,
    typed,

    -- * Multiplicity
    Multiplicity,
    once,
    anyMultiplicity,
    exactly,
    atLeast,
    atMost,
    interval,

    -- * Mock implementation
    Mockable (..),
    MatchResult (..),
    mockMethod,
  )
where

import Test.HMock.Internal.Multiplicity
import Test.HMock.Internal.Core
import Test.HMock.Internal.Predicates
