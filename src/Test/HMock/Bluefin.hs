{-# LANGUAGE DataKinds #-}

-- | This module makes HMock usable with the [Bluefin](https://hackage.haskell.org/package/bluefin)
-- effect system, alongside the mtl-style type classes supported by
-- "Test.HMock".
--
-- Where an mtl-style mock is indexed by a type class, a Bluefin mock is
-- indexed by a /capability/: a record of effectful operations, in the style
-- described by "Bluefin.Compound".  An operation is what the rest of HMock
-- calls a method.  A class has one instance per monad, but a
-- test can hold any number of capabilities of the same type, so expectations
-- here belong to a particular mocked instance.  A call through one
-- @FileSystem@ will not satisfy an expectation set on another.
--
-- @
-- data FileSystem e = MkFileSystem
--   { readFileImpl :: FilePath -> 'Bluefin.Eff.Eff' e String,
--     writeFileImpl :: FilePath -> String -> Eff e ()
--   }
--
-- 'makeMockableCapability' [t|FileSystem|]
--
-- test :: IO ()
-- test = 'Bluefin.Eff.runEff' '$' \\io -> 'runMock' io '$' \\source -> do
--   fs <- 'newMock' source
--   'expect' fs '$' ReadFile "foo.txt" '|->' "lorem ipsum"
--   codeUnderTest ('mocked' fs)
-- @
--
-- This module re-exports what you need to write such a test, including the
-- splice, so there's no need to import "Test.HMock" or
-- "Test.HMock.Bluefin.TH" alongside it.
--
-- == Threads
--
-- A source and its mocks may be used from any thread.  Expectations are
-- shared, so a thread can add expectations that calls from another thread
-- satisfy, and calls from several threads are matched against one plan.
-- 'Bluefin.IO.withEffToIO_' supplies the unlifting function that
-- 'Control.Concurrent.forkIO' needs:
--
-- @
-- 'Bluefin.IO.withEffToIO_' io '$' \\unlift -> 'Control.Concurrent.forkIO' '$' unlift '$' do
--   'expect' fs '$' ReadFile "b.txt" '|->' "from thread"
--   codeUnderTest ('mocked' fs)
-- @
--
-- 'runMock' verifies the expectations when its body returns, and a nested
-- block does the same at its end, so wait for any thread that adds or
-- satisfies expectations before then.  A response attached with '|=>' runs on
-- whichever thread made the matching call.
module Test.HMock.Bluefin
  ( -- * Running mocks
    MockSource,
    runMock,

    -- * Mock capabilities
    Mock,
    MockableCapability (..),
    newMock,
    newNamedMock,
    mockName,

    -- * Nested expectations
    nestMockSource,
    adoptMock,

    -- * Expectations
    Target (Accepts),
    ExpectContext (..),
    Expectation,
    ExpectMode (..),
    Expects,

    -- * Defaults and side effects
    byDefault,
    allowUnexpected,
    whenever,

    -- * Checking and configuring a test
    Severity (..),
    setAmbiguityCheck,
    setUninterestingActionCheck,
    setUnexpectedActionCheck,
    setUnmetExpectationCheck,
    describeExpectations,
    verifyExpectations,

    -- * Delegating operations to HMock
    mockMethod,
    mockDefaultlessMethod,
    withUnlift,

    -- * Rules and responses
    Rule,
    Expectable (..),
    (|->),
    (|=>),
    WholeMethodMatcher (SuchThat),

    -- * Template Haskell generator

    -- | The splice that generates the boilerplate for a capability.
    module Test.HMock.Bluefin.TH,

    -- * Re-exports
    module Test.HMock.Mockable,
    module Test.HMock.Multiplicity,
  )
where

import Test.HMock.Bluefin.Mock
import Test.HMock.Bluefin.TH
import Test.HMock.Mockable
import Test.HMock.Multiplicity
