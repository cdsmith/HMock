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
-- 'Test.HMock.TH.makeMockable' [t|MonadFilesystem|]
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
  ( 
    -- * The 'Mockable' class

    -- | HMock starts with the 'Mockable' class (most of which is actually in
    -- its superclass, 'MockableBase').  This class is implemented for each
    -- interface you want to mock, and describes which actions are possible,
    -- and how to match and compare them.  It's a lot of boilerplate, so you'll
    -- usually derive it with Template Haskell, but the instance must exist.
    module Test.HMock.Mockable,

    -- * Running mocks

    -- | Tests with mocks run in the 'MockT' monad transformer, which wraps a
    -- base monad and adds the ability to delegate methods to HMock for
    -- matching.  'runMockT' is the entry point for 'MockT'.
    --
    -- This module also defines the more restricted 'MockSetup;' monad, which
    -- is used to set up defaults for a type.
    module Test.HMock.MockT,

    -- * Rules for actions and responses

    -- | The bread and butter of mocks is matching actions and specifying
    -- responses.  Matchers and corresponding responses are combined into a
    -- 'Rule'
    module Test.HMock.Rule,

    -- * Combinators for building test plans

    -- | A complete execution plans consists of a collection of individual rules
    -- combined in various ways.  HMock defines a set of composable combinators
    -- for the execution plan.
    module Test.HMock.ExpectContext,

    -- * Multiplicity

    -- | For repeated actions in your execution plan, you often want to control
    -- the number of times somrthing is allowed to happen.  This is called a
    -- 'Multiplicity'.
    module Test.HMock.Multiplicity,

    -- * Delegating mocks

    -- | In order to run your test code with the 'MockT', you need instances of
    -- your effect classes for the 'MockT' type.  If you mock all methods of the
    -- class, this can be derived using Template Haskell.  For partial mocks,
    -- you'll need to write the instances yourself, using 'mockMethod' and its
    -- cousin 'mockDefaultlessMethod'.
    module Test.HMock.MockMethod,

    -- * Template Haskell generator

    -- | These are the Template Haskell splices which generate boilerplate for
    -- your classes to be used with HMock.
    module Test.HMock.TH,
  )
where

import Test.HMock.ExpectContext
import Test.HMock.MockT
import Test.HMock.MockMethod
import Test.HMock.Mockable
import Test.HMock.Multiplicity
import Test.HMock.Rule
import Test.HMock.TH
