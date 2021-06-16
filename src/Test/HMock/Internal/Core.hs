{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.HMock.Internal.Core where

import Control.Arrow (second)
import Control.Concurrent (MVar)
import Control.Monad.Base (MonadBase)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Cont (MonadCont)
import Control.Monad.Except (MonadError)
import Control.Monad.RWS (MonadRWS)
import Control.Monad.Reader
  ( MonadReader (ask, local, reader),
    ReaderT,
    mapReaderT,
    runReaderT,
  )
import Control.Monad.State (MonadState)
import Control.Monad.Trans (MonadIO, MonadTrans, lift)
import Control.Monad.Writer (MonadWriter)
import Data.Constraint (Constraint)
import Data.Default (Default (def))
import Data.Either (partitionEithers)
import Data.Function (on)
import Data.Kind (Type)
import Data.List (intercalate, sortBy)
import Data.Maybe (catMaybes, listToMaybe)
import Data.Typeable (Typeable, cast)
import GHC.Stack (CallStack, HasCallStack, callStack, withFrozenCallStack)
import GHC.TypeLits (KnownSymbol, Symbol)
import Test.HMock.Internal.Multiplicity
  ( Multiplicity (..),
    anyMultiplicity,
    decMultiplicity,
    exactly,
    exhaustable,
    once,
  )
import Test.HMock.Internal.Util (Located (..), locate, withLoc)
import UnliftIO (MonadUnliftIO, newMVar, putMVar, readMVar, takeMVar)

-- | The result of matching a @'Matcher' a@ with an @'Action' b@.  Because the
-- types should already guarantee that the methods match, all that's left is to
-- match arguments.
data MatchResult where
  -- | No match.  The int is the number of arguments that don't match.
  NoMatch :: Int -> MatchResult
  -- | Match. Stores a witness to the equality of return types.
  Match :: MatchResult

-- | A class for 'Monad' subclasses whose methods can be mocked.  You usually
-- want to generate this instance using 'Test.HMock.TH.makeMockable' or
-- 'Test.HMock.TH.deriveMockable', because it's just a lot of boilerplate.
class Typeable cls => Mockable (cls :: (Type -> Type) -> Constraint) where
  -- | An action that is performed.  This data type will have one constructor
  -- for each method.
  data Action cls :: Symbol -> (Type -> Type) -> Type -> Type

  -- | A specification for matching actions.  The actual arguments should be
  -- replaced with predicates.
  data Matcher cls :: Symbol -> (Type -> Type) -> Type -> Type

  -- | Gets a text description of an 'Action', for use in error messages.
  showAction :: Action cls name m a -> String

  -- | Gets a text description of a 'Matcher', for use in error messages.
  showMatcher :: Maybe (Action cls name m a) -> Matcher cls name m b -> String

  -- | Attempts to match an 'Action' with a 'Matcher'.
  matchAction :: Matcher cls name m a -> Action cls name m a -> MatchResult

-- | Something that can be expected.  This type class covers a number of cases:
--
--   * Expecting an exact 'Action'.
--   * Expecting anything that matches a 'Matcher'.
--   * Adding a return value (with '|->') or response (with '|=>').
class Expectable cls name m r e | e -> cls name m r where
  toRule :: e -> Rule cls name m r

-- | A rule for matching a method and responding to it when it matches.
--
-- The method may be matched by providing either an 'Action' to match exactly,
-- or a 'Matcher'.  Exact matching is only available when all method arguments
--
--
-- A 'Rule' may have zero or more responses, which are attached using '|->' and
-- '|=>'.  If there are no responses for a 'Rule', then there must be a default
-- response for that action, and it is used.  If more than one response is
-- added, the rule will perform the responses in order, repeating the last
-- response if there are additional matches.
--
-- Example:
--
-- @
-- 'expect' $
--   GetLine_ 'Test.HMock.anything'
--     '|->' "hello"
--     '|=>' \(GetLine prompt) -> "The prompt was " ++ prompt
--     '|->' "quit"
-- @
data
  Rule
    (cls :: (Type -> Type) -> Constraint)
    (name :: Symbol)
    (m :: Type -> Type)
    (r :: Type)
  where
  (:=>) ::
    Matcher cls name m r ->
    [Action cls name m r -> MockT m r] ->
    Rule cls name m r

-- | Attaches a response to an expectation.  This is a very flexible response,
-- which can look at arguments, do things in the base monad, set up more
-- expectations, etc.  The matching 'Action' is passed to the response, and is
-- guaranteed to be a match so it's fine to just pattern match on the correct
-- method.
(|=>) ::
  Expectable cls name m r expectable =>
  expectable ->
  (Action cls name m r -> MockT m r) ->
  Rule cls name m r
e |=> r = m :=> (rs ++ [r]) where m :=> rs = toRule e

infixl 1 |=>

-- | Attaches a return value to an expectation.  This is more convenient than
-- '|=>' in the common case where you just want to return a known result.
-- @e '|->' r@ means the same thing as @e '|=>' 'const' ('return' r)@.
(|->) ::
  (Monad m, Expectable cls name m r expectable) =>
  expectable ->
  r ->
  Rule cls name m r
m |-> r = m |=> const (return r)

infixl 1 |->

instance Expectable cls name m r (Rule cls name m r) where
  toRule = id

instance Expectable cls name m r (Matcher cls name m r) where
  toRule m = m :=> []

-- | All constraints needed to mock a method with the given class, name, base
-- monad, and return type.
type MockableMethod
  (cls :: (Type -> Type) -> Constraint)
  (name :: Symbol)
  (m :: Type -> Type)
  (r :: Type) =
  (Mockable cls, Typeable m, KnownSymbol name, Typeable r)

-- | A single step of an expectation.
data Step where
  Step :: MockableMethod cls name m r => Located (Rule cls name m r) -> Step

data Order = InOrder | AnyOrder deriving (Eq)

-- | A set of expected actions and their responses.  An entire test with mocks
-- is expected to run in a single base 'Monad', which is the first type
-- parameter here.  The second parameter is just a trick with `ExpectContext`
-- (see below) to avoid GHC warnings about unused return values.
data ExpectSet (m :: Type -> Type) a where
  ExpectNothing :: ExpectSet m ()
  Expect :: Multiplicity -> Step -> ExpectSet m ()
  ExpectMulti :: Order -> [ExpectSet m ()] -> ExpectSet m ()

-- | Converts a set of expectations into a string that summarizes them, with
-- the given prefix (used to indent).
formatExpectSet :: String -> ExpectSet m () -> String
formatExpectSet prefix ExpectNothing = prefix ++ "nothing"
formatExpectSet prefix (Expect multiplicity (Step l@(Loc _ (m :=> _)))) =
  prefix ++ withLoc (showMatcher Nothing m <$ l) ++ mult
  where
    mult
      | multiplicity == once = ""
      | otherwise = " [" ++ show multiplicity ++ "]"
formatExpectSet prefix (ExpectMulti order xs) =
  let label = if order == InOrder then "in sequence:\n" else "in any order:\n"
   in prefix ++ label ++ unlines (map (formatExpectSet (prefix ++ "  ")) xs)

-- | Get a list of steps that can match actions right now, together with the
-- remaining expectations if each one were to match.
liveSteps :: ExpectSet m () -> [(Step, ExpectSet m ())]
liveSteps = map (second simplify) . go
  where
    go ExpectNothing = []
    go (Expect multiplicity step) = case decMultiplicity multiplicity of
      Nothing -> [(step, ExpectNothing)]
      Just multiplicity' -> [(step, Expect multiplicity' (nextResponse step))]
    go (ExpectMulti order es) = fmap (ExpectMulti order) <$> nextSteps order es

    nextSteps _ [] = []
    nextSteps order (e : es)
      | AnyOrder <- order = eOptions ++ map (fmap (e :)) esOptions
      | ExpectNothing <- excess e = eOptions ++ esOptions
      | otherwise = eOptions
      where
        eOptions = fmap (: es) <$> go e
        esOptions = nextSteps order es

    nextResponse (Step (Loc l (m :=> (_ : r : rs)))) =
      Step (Loc l (m :=> (r : rs)))
    nextResponse other = other

-- | Simplifies a set of expectations.  This removes unnecessary occurrences of
-- 'ExpectNothing' and collapses nested lists with the same ordering
-- constraints.
simplify :: ExpectSet m () -> ExpectSet m ()
simplify e = case e of
  (ExpectMulti order xs) -> simplifyMulti order xs
  _ -> e
  where
    simplifyMulti order =
      construct order . concatMap (expand order . simplify)

    expand :: Order -> ExpectSet m () -> [ExpectSet m ()]
    expand _ ExpectNothing = []
    expand order (ExpectMulti order' xs) | order == order' = xs
    expand _ other = [other]

    construct _ [] = ExpectNothing
    construct _ [x] = x
    construct order xs = ExpectMulti order xs

-- | Reduces a set of expectations to the minimum steps that would be required
-- to satisfy the entire set.  This weeds out unnecessary information before
-- reporting that there were unmet expectations at the end of the test.
excess :: ExpectSet m () -> ExpectSet m ()
excess = simplify . go
  where
    go :: ExpectSet m () -> ExpectSet m ()
    go ExpectNothing = ExpectNothing
    go e@(Expect mult _)
      | exhaustable mult = ExpectNothing
      | otherwise = e
    go (ExpectMulti order xs) = ExpectMulti order (map go xs)

-- | Type class for contexts in which it makes sense to express an expectation.
-- Notably, this includes `MockT`, which expects actions to be performed during
-- a test.
class ExpectContext (t :: (Type -> Type) -> Type -> Type) where
  fromExpectSet :: MonadIO m => ExpectSet m () -> t m ()

instance ExpectContext ExpectSet where
  fromExpectSet = id

makeExpect ::
  (Expectable cls name m r expectable, MockableMethod cls name m r) =>
  CallStack ->
  Multiplicity ->
  expectable ->
  ExpectSet m ()
makeExpect cs mult e = Expect mult (Step (locate cs (toRule e)))

-- | Creates an expectation that an action is performed once per given
-- response (or exactly once if there is no response).
--
-- @
-- 'runMockT' '$' do
--   'expect' '$'
--     ReadFile "foo.txt"
--       '|->' "lorem ipsum"
--       '|->' "oops, the file changed out from under me!"
--   callCodeUnderTest
-- @
--
-- In this example, `readFile` must be called exactly twice by the tested code,
-- and will return "lorem ipsum" the first time, but something different the
-- second time.
expect ::
  ( HasCallStack,
    MonadIO m,
    MockableMethod cls name m r,
    Expectable cls name m r expectable,
    ExpectContext ctx
  ) =>
  expectable ->
  ctx m ()
expect e = fromExpectSet (makeExpect callStack (exactly (max 1 (length rs))) e)
  where
    (_ :=> rs) = toRule e

-- | Creates an expectation that an action is performed some number of times.
--
-- @
--   'runMockT' '$' do
--     'expect' '$' MakeList '|->' ()
--     'expectN' ('Test.HMock.atLeast' 2) '$'
--       CheckList "Cindy Lou Who" '|->' "nice"
--
--     callCodeUnderTest
-- @
expectN ::
  ( HasCallStack,
    MonadIO m,
    MockableMethod cls name m r,
    Expectable cls name m r expectable,
    ExpectContext ctx
  ) =>
  -- | The number of times the action should be performed.
  Multiplicity ->
  -- | The action and its response.
  expectable ->
  ctx m ()
expectN mult e
  | Multiplicity _ (Just n) <- mult,
    length rs > n =
    error "Too many responses for the specified multiplicity"
  | otherwise = fromExpectSet (makeExpect callStack mult e)
  where
    (_ :=> rs) = toRule e

-- | Specifies a response if a matching action is performed, but doesn't expect
-- anything.  This is equivalent to @'expectN' 'anyMultiplicity'@, but shorter.
--
-- In this example, the later use of 'whenever' overrides earlier uses, but only
-- for calls that match its conditions.
--
-- @
--   'runMockT' '$' do
--     'whenever' '$' ReadFile_ anything '|->' "tlhIngan maH!"
--     'whenever' '$' ReadFile "config.txt" '|->' "lang: klingon"
--
--     callCodeUnderTest
-- @
whenever ::
  ( HasCallStack,
    MonadIO m,
    MockableMethod cls name m r,
    Expectable cls name m r expectable,
    ExpectContext ctx
  ) =>
  expectable ->
  ctx m ()
whenever = fromExpectSet . makeExpect callStack anyMultiplicity

-- | Creates a sequential expectation.  Other actions can still happen during
-- the sequence, but these specific expectations must be met in this order.
--
-- @
--   'inSequence'
--     [ 'expect' '$' MoveForward '|->' (),
--       'expect' '$' TurnRight '|->' (),
--       'expect' '$' MoveForward '|->' ()
--     ]
-- @
--
-- Beware of using 'inSequence' too often.  It is appropriate when the property
-- you are testing is that the order of effects is correct.  If that's not the
-- purpose of the test, consider adding several independent expectations,
-- instead.  This avoids over-asserting, and keeps your tests less brittle.
inSequence ::
  (MonadIO m, ExpectContext ctx) =>
  (forall ctx'. ExpectContext ctx' => [ctx' m ()]) ->
  ctx m ()
inSequence es = fromExpectSet (ExpectMulti InOrder es)

-- | Combines multiple expectations, which can occur in any order.  Most of the
-- time, you can achieve the same thing by expecting each separately, but this
-- can be combined with 'inSequence' to describe more complex ordering
-- constraints, such as:
--
-- @
--   'inSequence'
--     [ 'inAnyOrder'
--         [ 'expect' '$' adjustMirrors '|->' (),
--           'expect' '$' fastenSeatBelt '|->' ()
--         ],
--       'expect' '$' startCar '|->' ()
--     ]
-- @
inAnyOrder ::
  (MonadIO m, ExpectContext ctx) =>
  (forall ctx'. ExpectContext ctx' => [ctx' m ()]) ->
  ctx m ()
inAnyOrder es = fromExpectSet (ExpectMulti AnyOrder es)

-- | Monad transformer for running mocks.
newtype MockT m a where
  MockT :: {unMockT :: ReaderT (MVar (ExpectSet m ())) m a} -> MockT m a
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadFail,
      MonadIO,
      MonadState s,
      MonadWriter w,
      MonadRWS r w s,
      MonadError e,
      MonadCont,
      MonadBase b,
      MonadCatch,
      MonadMask,
      MonadThrow,
      MonadUnliftIO
    )

instance MonadTrans MockT where
  lift = MockT . lift

mapMockT :: (m a -> m b) -> MockT m a -> MockT m b
mapMockT f = MockT . mapReaderT f . unMockT

instance MonadReader r m => MonadReader r (MockT m) where
  ask = lift ask
  local = mapMockT . local
  reader = lift . reader

instance ExpectContext MockT where
  fromExpectSet e = do
    expectVar <- MockT ask
    expectSet <- takeMVar expectVar
    putMVar expectVar (simplify (ExpectMulti AnyOrder [e, expectSet]))

-- | Runs a test in the 'MockT' monad, handling all of the mocks.
runMockT :: MonadIO m => MockT m a -> m a
runMockT test = withMockT (\_ -> test)

-- | Runs a test in the 'MockT' monad.  The test can unlift other MockT pieces
-- to the base monad while still acting on the same set of expectations.  This
-- can be useful for testing concurrency or similar mechanisms.
--
-- @
-- test = 'withMockT' '$' \inMockT -> do
--    'expect' '$' ...
--
--    'liftIO' '$' 'forkIO' '$' inMockT firstThread
--    'liftIO' '$' 'forkIO' '$' inMockT secondThread
-- @
--
-- This is a low-level primitive.  Consider using the @unliftio@ package for
-- higher level implementations of multithreading and other primitives.
withMockT ::
  forall m b. MonadIO m => ((forall a. MockT m a -> m a) -> MockT m b) -> m b
withMockT test = do
  eVar <- newMVar ExpectNothing
  let inMockT :: forall a. MockT m a -> m a
      inMockT m = runReaderT (unMockT m) eVar
  flip runReaderT eVar $
    unMockT $ do
      a <- test inMockT
      verifyExpectations
      return a

-- | Fetches a 'String' that describes the current set of outstanding
-- expectations.  This is sometimes useful for debugging test code.  The exact
-- format is not specified.
describeExpectations :: MonadIO m => MockT m String
describeExpectations = formatExpectSet "" <$> (MockT ask >>= readMVar)

-- | Verifies that all mock expectations are satisfied.  You normally don't need
-- to do this, because it happens automatically at the end of your test in
-- 'runMockT'.  However, it's occasionally useful to check expectations in the
-- middle of a test, such as before going on to the next stage.
--
-- Use of 'verifyExpectations' might signify that you are doing too much in a
-- single test.  Consider splitting large tests into a separate test for each
-- case.
verifyExpectations :: MonadIO m => MockT m ()
verifyExpectations = do
  expectSet <- MockT ask >>= readMVar
  case excess expectSet of
    ExpectNothing -> return ()
    missing -> error $ "Unmet expectations:\n" ++ formatExpectSet "  " missing

mockMethodImpl ::
  forall cls name m r.
  ( HasCallStack,
    MonadIO m,
    MockableMethod cls name m r
  ) =>
  Bool ->
  MockT m r ->
  Action cls name m r ->
  MockT m r
mockMethodImpl lax surrogate action =
  do
    expectVar <- MockT ask
    expectSet <- takeMVar expectVar
    let (newExpectSet, response) = decideAction expectSet
    putMVar expectVar newExpectSet
    response
  where
    decideAction expectSet =
      let (partial, full) = partitionEithers (tryMatch <$> liveSteps expectSet)
          orderedPartial = snd <$> sortBy (compare `on` fst) (catMaybes partial)
       in case (full, surrogate, orderedPartial) of
            ((e, Just response) : _, _, _) -> (e, response)
            ((e, Nothing) : _, response, _) -> (e, response)
            ([], response, _) | lax -> (expectSet, response)
            ([], _, []) -> error $ noMatchError action
            ([], _, _) -> error $ partialMatchError action orderedPartial
    tryMatch ::
      (Step, ExpectSet m ()) ->
      Either (Maybe (Int, String)) (ExpectSet m (), Maybe (MockT m r))
    tryMatch (Step expected, e)
      | Just lrule@(Loc _ (m :=> impls)) <- cast expected =
        case matchAction m action of
          NoMatch n ->
            Left (Just (n, withLoc (showMatcher (Just action) m <$ lrule)))
          Match ->
            Right (e, listToMaybe impls <*> Just action)
      | otherwise = Left Nothing

-- | Implements a method in a 'Mockable' monad by delegating to the mock
-- framework.  If the method is called unexpectedly, an exception will be
-- thrown.  However, an expected invocation without a specified response will
-- return the default value.
mockMethod ::
  forall cls name m r.
  ( HasCallStack,
    MonadIO m,
    MockableMethod cls name m r,
    Default r
  ) =>
  Action cls name m r ->
  MockT m r
mockMethod action =
  withFrozenCallStack $ mockMethodImpl False (return def) action

-- | Implements a method in a 'Mockable' monad by delegating to the mock
-- framework.  If the method is used unexpectedly, the default value will be
-- returned.  This is called a lax mock.
mockLaxMethod ::
  forall cls name m r.
  ( HasCallStack,
    MonadIO m,
    MockableMethod cls name m r,
    Default r
  ) =>
  Action cls name m r ->
  MockT m r
mockLaxMethod action =
  withFrozenCallStack $ mockMethodImpl True (return def) action

-- | Implements a method in a 'Mockable' monad by delegating to the mock
-- framework.  If the method is called unexpectedly, an exception will be
-- thrown.  However, an expected invocation without a specified response will
-- return undefined.  This can be used in place of 'mockMethod' when the return
-- type has no default.
mockDefaultlessMethod ::
  forall cls name m r.
  ( HasCallStack,
    MonadIO m,
    MockableMethod cls name m r
  ) =>
  Action cls name m r ->
  MockT m r
mockDefaultlessMethod action =
  withFrozenCallStack $ mockMethodImpl False (return undefined) action

-- | Implements a method in a 'Mockable' monad by delegating to the mock
-- framework.  If the method is used unexpectedly, undefined will be returned.
-- This can be used in place of 'mockLaxMethod' when the return type has no
-- default.
mockLaxDefaultlessMethod ::
  forall cls name m r.
  ( HasCallStack,
    MonadIO m,
    MockableMethod cls name m r
  ) =>
  Action cls name m r ->
  MockT m r
mockLaxDefaultlessMethod action =
  withFrozenCallStack $ mockMethodImpl True (return undefined) action

-- An error for an action that matches no expectations at all.
noMatchError ::
  (HasCallStack, Mockable cls) =>
  -- | The action that was received.
  Action cls name m a ->
  String
noMatchError a =
  "Unexpected action: " ++ showAction a

-- An error for an action that matches no expectations at all.
noResponseError ::
  (HasCallStack, Mockable cls) =>
  -- | The action that was received.
  Action cls name m a ->
  String
noResponseError a =
  "Expectation lacks a response and has no default: " ++ showAction a

-- An error for an action that doesn't match the argument predicates for any
-- of the method's expectations.
partialMatchError ::
  (HasCallStack, Mockable cls) =>
  -- | The action that was received.
  Action cls name m a ->
  -- | Descriptions of the matchers that most closely matched, closest first.
  [String] ->
  String
partialMatchError a partials =
  "Wrong arguments: "
    ++ showAction a
    ++ "\n\nClosest matches:\n - "
    ++ intercalate "\n - " (take 5 partials)
