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
    byDefault,

    -- * Setting expectations
    MockableMethod,
    Expectable (..),
    Rule,
    (|=>),
    (|->),
    ExpectContext,
    expect,
    expectN,
    expectAny,
    inSequence,
    inAnyOrder,

    -- * Predicates
    Predicate (..),
    anything,
    eq,
    neq,
    gt,
    geq,
    lt,
    leq,
    just,
    left,
    right,
    zipP,
    zip3P,
    zip4P,
    zip5P,
    andP,
    orP,
    notP,
    startsWith,
    endsWith,
    hasSubstr,
    hasSubsequence,
    caseInsensitive,
    matchesRegex,
    matchesCaseInsensitiveRegex,
    containsRegex,
    containsCaseInsensitiveRegex,
    isEmpty,
    nonEmpty,
    sizeIs,
    elemsAre,
    unorderedElemsAre,
    each,
    contains,
    containsAll,
    containsOnly,
    containsKey,
    containsEntry,
    keysAre,
    entriesAre,
    approxEq,
    finite,
    infinite,
    nAn,
    is,
    qIs,
    with,
    qWith,
    qMatch,
    typed,

    -- * Multiplicity
    Multiplicity,
    meetsMultiplicity,
    once,
    anyMultiplicity,
    exactly,
    atLeast,
    atMost,
    interval,

    -- * Implementing mocks
    Mockable (..),
    MockableSetup (..),
    MatchResult (..),
    mockMethod,
    mockDefaultlessMethod,
  )
where

import Test.HMock.Internal.Expectable
import Test.HMock.Internal.MockT
import Test.HMock.Internal.Mockable
import Test.HMock.Internal.Multiplicity
import Test.HMock.Internal.Predicates

instance {-# OVERLAPPABLE #-} MockableSetup cls
