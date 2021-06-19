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
import Control.Monad (unless)
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
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import Data.Proxy (Proxy (Proxy))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (TypeRep, Typeable, cast, typeRep)
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

#if !MIN_VERSION_base(4, 13, 0)
import Control.Monad.Fail (MonadFail)
#endif

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
class
  (Typeable cls, MockableSetup cls) =>
  Mockable (cls :: (Type -> Type) -> Constraint)
  where
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

-- | A class that can be used to set up default behaviors for a 'Mockable'
-- class.  There is a default instance that does nothing, but you can derive
-- your own instances that overlap that one and add setup behavior.
class MockableSetup (cls :: (Type -> Type) -> Constraint) where
  -- An action to run and set up defaults for this class.  The action will be
  -- run before HMock touches the class, either to add expectations or to
  -- delegate a method.
  setupMockable :: (MonadIO m, Typeable m) => proxy cls -> MockT m ()

instance {-# OVERLAPPABLE #-} Mockable cls => MockableSetup cls where
  setupMockable _ = return ()

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
data Step m where
  Step :: MockableMethod cls name m r => Located (Rule cls name m r) -> Step m

data Order = InOrder | AnyOrder deriving (Eq)

-- | A set of expected actions and their responses.  An entire test with mocks
-- is expected to run in a single base 'Monad', which is the first type
-- parameter here.  The second parameter is just a trick with `ExpectContext`
-- (see below) to avoid GHC warnings about unused return values.
data ExpectSet (m :: Type -> Type) a where
  ExpectNothing :: ExpectSet m ()
  Expect :: Multiplicity -> Step m -> ExpectSet m ()
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
liveSteps :: ExpectSet m () -> [(Step m, ExpectSet m ())]
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
expectAny ::
  ( HasCallStack,
    MonadIO m,
    MockableMethod cls name m r,
    Expectable cls name m r expectable,
    ExpectContext ctx
  ) =>
  expectable ->
  ctx m ()
expectAny = fromExpectSet . makeExpect callStack anyMultiplicity

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

data MockState m = MockState
  { mockExpectSet :: ExpectSet m (),
    mockDefaults :: [Step m],
    mockClasses :: Set TypeRep
  }

initMockState :: MockState m
initMockState =
  MockState
    { mockExpectSet = ExpectNothing,
      mockDefaults = [],
      mockClasses = Set.empty
    }

-- | Monad transformer for running mocks.
newtype MockT m a where
  MockT :: {unMockT :: ReaderT (MVar (MockState m)) m a} -> MockT m a
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

initClassIfNeeded ::
  forall cls m proxy.
  (Mockable cls, Typeable m, MonadIO m) =>
  proxy cls ->
  MockT m ()
initClassIfNeeded proxy =
  do
    stateVar <- MockT ask
    mockState <- takeMVar stateVar
    let newMockClasses = Set.insert t (mockClasses mockState)
    putMVar stateVar (mockState {mockClasses = newMockClasses})
    unless (t `Set.member` mockClasses mockState) $ do
      setupMockable (Proxy :: Proxy cls)
  where
    t = typeRep proxy

initClassesAsNeeded :: MonadIO m => ExpectSet m () -> MockT m ()
initClassesAsNeeded ExpectNothing = return ()
initClassesAsNeeded (ExpectMulti _ es) = mapM_ initClassesAsNeeded es
initClassesAsNeeded (Expect _ (Step (_ :: Located (Rule cls name m r)))) =
  initClassIfNeeded (Proxy :: Proxy cls)

instance MonadReader r m => MonadReader r (MockT m) where
  ask = lift ask
  local = mapMockT . local
  reader = lift . reader

instance ExpectContext MockT where
  fromExpectSet e = do
    initClassesAsNeeded e
    stateVar <- MockT ask
    mockState <- takeMVar stateVar
    let newExpectSet = simplify (ExpectMulti AnyOrder [e, mockExpectSet mockState])
    putMVar stateVar (mockState {mockExpectSet = newExpectSet})

-- | Runs a test in the 'MockT' monad, handling all of the mocks.
runMockT :: forall m a. MonadIO m => MockT m a -> m a
runMockT test = withMockT constTest
  where
    constTest :: (forall b. MockT m b -> m b) -> MockT m a
    constTest _inMockT = test

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
  stateVar <- newMVar initMockState
  let inMockT :: forall a. MockT m a -> m a
      inMockT m = runReaderT (unMockT m) stateVar
  flip runReaderT stateVar $
    unMockT $ do
      a <- test inMockT
      verifyExpectations
      return a

-- | Fetches a 'String' that describes the current set of outstanding
-- expectations.  This is sometimes useful for debugging test code.  The exact
-- format is not specified.
describeExpectations :: MonadIO m => MockT m String
describeExpectations =
  formatExpectSet "" <$> (MockT ask >>= fmap mockExpectSet . readMVar)

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
  expectSet <- MockT ask >>= fmap mockExpectSet . readMVar
  case excess expectSet of
    ExpectNothing -> return ()
    missing -> error $ "Unmet expectations:\n" ++ formatExpectSet "  " missing

-- | Changes the default response for matching actions.
--
-- Without 'byDefault', actions with no explicit response will return the
-- 'Default' value for the type, or 'undefined' if the return type isn't an
-- instance of 'Default'.  'byDefault' replaces that with a new default
-- response, also overriding any previous defaults. The rule passed in must have
-- exactly one response.
byDefault ::
  forall cls name m r.
  (MonadIO m, MockableMethod cls name m r) =>
  Rule cls name m r ->
  MockT m ()
byDefault r@(_ :=> [_]) = do
  initClassIfNeeded (Proxy :: Proxy cls)
  stateVar <- MockT ask
  mockState <- takeMVar stateVar
  let newDefaults = Step (locate callStack r) : mockDefaults mockState
  putMVar stateVar (mockState {mockDefaults = newDefaults})
byDefault _ = error "Defaults must have exactly one response."

mockMethodImpl ::
  forall cls name m r.
  ( HasCallStack,
    MonadIO m,
    MockableMethod cls name m r
  ) =>
  Bool ->
  r ->
  Action cls name m r ->
  MockT m r
mockMethodImpl lax surrogate action =
  do
    initClassIfNeeded (Proxy :: Proxy cls)
    stateVar <- MockT ask
    mockState <- takeMVar stateVar
    let (newExpectSet, response) =
          decideAction (mockExpectSet mockState) (mockDefaults mockState)
    putMVar stateVar mockState {mockExpectSet = newExpectSet}
    response
  where
    decideAction expectSet defaults =
      let (partial, full) = partitionEithers (tryMatch <$> liveSteps expectSet)
          orderedPartial = snd <$> sortBy (compare `on` fst) (catMaybes partial)
       in case (full, findDefault defaults, surrogate, orderedPartial) of
            ((e, Just response) : _, _, _, _) -> (e, response)
            ((e, Nothing) : _, d, s, _) -> (e, fromMaybe (return s) d)
            ([], d, s, _) | lax -> (expectSet, fromMaybe (return s) d)
            ([], _, _, []) -> error $ noMatchError action
            ([], _, _, _) -> error $ partialMatchError action orderedPartial
    tryMatch ::
      (Step m, ExpectSet m ()) ->
      Either (Maybe (Int, String)) (ExpectSet m (), Maybe (MockT m r))
    tryMatch (Step expected, e)
      | Just lrule@(Loc _ (m :=> impls)) <- cast expected =
        case matchAction m action of
          NoMatch n ->
            Left (Just (n, withLoc (showMatcher (Just action) m <$ lrule)))
          Match ->
            Right (e, listToMaybe impls <*> Just action)
      | otherwise = Left Nothing

    findDefault :: [Step m] -> Maybe (MockT m r)
    findDefault [] = Nothing
    findDefault (Step expected : _)
      | Just (Loc _ (m :=> [r])) <- cast expected,
        Match <- matchAction m action =
        Just (r action)
    findDefault (_ : steps) = findDefault steps

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
  withFrozenCallStack $ mockMethodImpl False def action

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
  withFrozenCallStack $ mockMethodImpl True def action

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
  withFrozenCallStack $ mockMethodImpl False undefined action

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
  withFrozenCallStack $ mockMethodImpl True undefined action

-- An error for an action that matches no expectations at all.
noMatchError ::
  (HasCallStack, Mockable cls) =>
  -- | The action that was received.
  Action cls name m a ->
  String
noMatchError a =
  "Unexpected action: " ++ showAction a

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
