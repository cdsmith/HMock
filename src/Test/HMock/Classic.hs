-- | Mocking for @mtl@-style type classes: tests run in the t'MockT' monad
-- transformer, and a class is mocked by generating an instance of it for
-- @MockT@.  "Test.HMock" is a re-export of this module.
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
-- 'Test.HMock.Classic.TH.makeMockable' [t|MonadFilesystem|]
--
-- spec = describe "copyFile" '$'
--   it "reads a file and writes its contents to another file" '$'
--     'runMockT' '$' do
--       'expect' '$' ReadFile "foo.txt" '|->' "contents"
--       'expect' '$' WriteFile "bar.txt" "contents" '|->' ()
--       copyFile "foo.txt" "bar.txt"
-- @
--
-- The Template Haskell splice, 'Test.HMock.Classic.TH.makeMockable', generates
-- the boilerplate needed to use @MonadFilesystem@ with HMock.  You then use
-- 'runMockT' to begin a test with mocks, 'expect' to set up your expected
-- actions and responses, and finally execute your code.
module Test.HMock.Classic
  ( 
    -- * The 'MockableBase' class

    -- | HMock starts with the 'MockableBase' class.  This class is implemented
    -- for each interface you want to mock, and describes which actions are
    -- possible, and how to match and compare them.  It's a lot of boilerplate,
    -- so you'll usually derive it with Template Haskell, but the instance must
    -- exist.  The 'Mockable' class, below, adds per-class setup on top of it.
    module Test.HMock.Mockable,

    -- * Running mocks

    -- | Tests with mocks run in the t'MockT' monad transformer, which wraps a
    -- base monad and adds the ability to delegate methods to HMock for
    -- matching.  'runMockT' is the entry point for t'MockT'.
    --
    -- This module also defines the more restricted t'MockSetup' monad, which
    -- is used to set up defaults for a type.
    module Test.HMock.Classic.MockT,

    -- * Rules for actions and responses

    -- | The bread and butter of mocks is matching actions and specifying
    -- responses.  Matchers and corresponding responses are combined into a
    -- 'Rule'
    module Test.HMock.Classic.Rule,

    -- * Combinators for building test plans

    -- | A complete execution plans consists of a collection of individual rules
    -- combined in various ways.  HMock defines a set of composable combinators
    -- for the execution plan.
    module Test.HMock.Classic.ExpectContext,

    -- * Defaults

    -- | What happens to a matching call when no expectation has anything to
    -- say about it.  These are also what you use from 'setupMockable' to give
    -- a class its defaults.
    module Test.HMock.Classic.Defaults,

    -- * Multiplicity

    -- | For repeated actions in your execution plan, you often want to control
    -- the number of times somrthing is allowed to happen.  This is called a
    -- t'Multiplicity'.
    module Test.HMock.Multiplicity,

    -- * Delegating mocks

    -- | In order to run your test code with the t'MockT', you need instances of
    -- your effect classes for the t'MockT' type.  If you mock all methods of the
    -- class, this can be derived using Template Haskell.  For partial mocks,
    -- you'll need to write the instances yourself, using 'mockMethod' and its
    -- cousin 'mockDefaultlessMethod'.
    module Test.HMock.Classic.MockMethod,

    -- * Template Haskell generator

    -- | These are the Template Haskell splices which generate boilerplate for
    -- your classes to be used with HMock.
    module Test.HMock.Classic.TH,
  )
where

import Test.HMock.Classic.Defaults
import Test.HMock.Classic.ExpectContext (ExpectContext (..))
import Test.HMock.Classic.MockMethod
import Test.HMock.Classic.MockT
import Test.HMock.Classic.Rule
import Test.HMock.Classic.TH
import Test.HMock.Mockable hiding (MockableMethod)
import Test.HMock.Multiplicity
