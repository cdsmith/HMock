{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.HMock.Internal.Core where

import Control.Monad.Base (MonadBase)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Cont (MonadCont)
import Control.Monad.Except (MonadError)
import Control.Monad.RWS (MonadRWS)
import Control.Monad.Reader (MonadReader)
import Control.Monad.State (MonadState, StateT, get, modify, put, runStateT)
import Control.Monad.Trans (MonadIO, MonadTrans, lift)
import Control.Monad.Writer (MonadWriter)
import Data.Constraint (Constraint)
import Data.Either (partitionEithers)
import Data.Function (on)
import Data.Kind (Type)
import Data.List (intercalate, sortBy)
import Data.Maybe (catMaybes)
import Data.Type.Equality (type (:~:) (..))
import Data.Typeable (Typeable, cast)
import GHC.Stack (CallStack, HasCallStack, callStack, withFrozenCallStack)
import GHC.TypeLits (KnownSymbol, Symbol)
import Test.HMock.Internal.Multiplicity
  ( Multiplicity,
    anyMultiplicity,
    decMultiplicity,
    exhaustable,
    once,
  )
import Test.HMock.Internal.Util (Loc, getSrcLoc, showWithLoc)

#if !MIN_VERSION_base(4, 13, 0)
import Control.Monad.Fail (MonadFail)
#endif

-- | The result of matching a @'Matcher' a@ with an @'Action' b@.  Because the
-- types should already guarantee that the methods match, all that's left is to
-- match arguments.
data MatchResult a b where
  -- | No match.  The int is the number of arguments that don't match.
  NoMatch :: Int -> MatchResult a b
  -- | Match. Stores a witness to the equality of return types.
  Match :: a :~: b -> MatchResult a b

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
  matchAction :: Matcher cls name m a -> Action cls name m b -> MatchResult a b

-- | A pair of a 'Matcher' and a response for when it matches.  The matching
-- 'Action' is passed to the response, and is guaranteed to be a match, so it's
-- okay to just pattern match on the correct method.
data
  Rule
    (cls :: (Type -> Type) -> Constraint)
    (name :: Symbol)
    (m :: Type -> Type)
  where
  -- | Matches an 'Action' and performs a response in the 'MockT' monad.  This
  -- is a vary flexible response, which can look at arguments, do things in the
  -- base monad, set up more expectations, etc.
  (:->) ::
    Matcher cls name m a ->
    (Action cls name m a -> MockT m a) ->
    Rule cls name m

-- | Matches an 'Action' and returns a constant response.  This is more
-- convenient than '(:->)' in the common case where you just want to return a
-- known result.
(|->) :: (Mockable cls, Monad m) => Matcher cls name m a -> a -> Rule cls name m
m |-> r = m :-> const (return r)

newtype Priority where
  Priority :: Int -> Priority
  deriving (Show, Eq, Ord)

lowPriority :: Priority
lowPriority = Priority 0

normalPriority :: Priority
normalPriority = Priority 1

-- | A single step of an expectation.
data Step where
  Step ::
    (Mockable cls, Typeable m, KnownSymbol name) =>
    Loc ->
    Rule cls name m ->
    Step

data Order = InOrder | AnyOrder deriving (Eq)

-- | A set of expected actions and their responses.  An entire test with mocks
-- is expected to run in a single base 'Monad', which is the first type
-- parameter here.  The second parameter is just a trick with `Expectable` (see
-- below) to avoid GHC warnings about unused return values.
data ExpectSet (m :: Type -> Type) a where
  ExpectNothing :: ExpectSet m ()
  Expect :: Priority -> Multiplicity -> Step -> ExpectSet m ()
  ExpectMulti :: Order -> [ExpectSet m ()] -> ExpectSet m ()

-- | Converts a set of expectations into a string that summarizes them, with
-- the given prefix (used to indent).
formatExpectSet :: String -> ExpectSet m () -> String
formatExpectSet prefix ExpectNothing = prefix ++ "nothing"
formatExpectSet prefix (Expect prio multiplicity (Step loc (m :-> _))) =
  prefix ++ showWithLoc loc (showMatcher Nothing m) ++ modifierDesc
  where
    modifiers = prioModifier ++ multModifier
    prioModifier
      | prio == lowPriority = ["low priority"]
      | otherwise = []
    multModifier
      | multiplicity == once = []
      | otherwise = [show multiplicity]
    modifierDesc
      | null modifiers = ""
      | otherwise = " (" ++ intercalate ", " modifiers ++ ")"
formatExpectSet prefix (ExpectMulti order xs) =
  let label = if order == InOrder then "in sequence:\n" else "in any order:\n"
   in prefix ++ label ++ unlines (map (formatExpectSet (prefix ++ "  ")) xs)

-- | Get a list of steps that can match actions right now, together with the
-- remaining expectations if each one were to match.
liveSteps :: ExpectSet m () -> [(Priority, Step, ExpectSet m ())]
liveSteps = map (\(p, s, e) -> (p, s, simplify e)) . go
  where
    go :: ExpectSet m () -> [(Priority, Step, ExpectSet m ())]
    go ExpectNothing = []
    go (Expect prio multiplicity step) = case decMultiplicity multiplicity of
      Nothing -> [(prio, step, ExpectNothing)]
      Just multiplicity' -> [(prio, step, Expect prio multiplicity' step)]
    go (ExpectMulti order es) =
      [ (p, a, ExpectMulti order (e' : es'))
        | (e, es') <- choices es,
          (p, a, e') <- go e
      ]
      where
        choices [] = []
        choices (x : xs)
          | order == InOrder, ExpectNothing <- excess x = (x, xs) : choices xs
          | order == InOrder = [(x, xs)]
          | otherwise = (x, xs) : (fmap (x :) <$> choices xs)

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
    go e@(Expect _ mult _)
      | exhaustable mult = ExpectNothing
      | otherwise = e
    go (ExpectMulti order xs) = ExpectMulti order (map go xs)

-- | Type class for types that can represent expectations for mocks.  The only
-- instance you need worry about is `MockT`, which expects actions to be
-- performed during a test.
class Expectable (t :: (Type -> Type) -> Type -> Type) where
  fromExpectSet :: Monad m => ExpectSet m () -> t m ()

instance Expectable ExpectSet where
  fromExpectSet = id

makeExpect ::
  (Mockable cls, Typeable m, KnownSymbol name) =>
  CallStack ->
  Priority ->
  Multiplicity ->
  Rule cls name m ->
  ExpectSet m ()
makeExpect cs prio mult wr = Expect prio mult (Step (getSrcLoc cs) wr)

-- | Creates an expectation that an action is performed once.  This is
-- equivalent to @'expectN' 'once'@, but shorter.
--
-- @
--   'runMockT' '$' do
--     'expect' '$' readFile_ "foo.txt" '|->' "lorem ipsum"
--     callCodeUnderTest
-- @
expect ::
  ( HasCallStack,
    Mockable cls,
    Typeable m,
    Monad m,
    KnownSymbol name,
    Expectable t
  ) =>
  Rule cls name m ->
  t m ()
expect = fromExpectSet . makeExpect callStack normalPriority once

-- | Creates an expectation that an action is performed some number of times.
--
-- @
--   'runMockT' '$' do
--     'expect' '$' makeList_ '|->' ()
--     'expectN' ('Test.HMock.atLeast' 2) '$'
--       checkList_ "Cindy Lou Who" '|->' "nice"
--
--     callCodeUnderTest
-- @
expectN ::
  ( HasCallStack,
    Mockable cls,
    Typeable m,
    Monad m,
    KnownSymbol name,
    Expectable t
  ) =>
  -- | The number of times the action should be performed.
  Multiplicity ->
  -- | The action and its response.
  Rule cls name m ->
  t m ()
expectN = (fromExpectSet .) . makeExpect callStack normalPriority

-- | Creates an expectation that an action is performed any number of times.
-- This is equivalent to @'expectN' 'anyMultiplicity'@, but shorter.
--
-- @
--   'runMockT' '$' do
--     'expectAny' '$' FetchFullName_ anything '|->' "John Doe"
--
--     callCodeUnderTest
-- @
expectAny ::
  ( HasCallStack,
    Mockable cls,
    Typeable m,
    Monad m,
    KnownSymbol name,
    Expectable t
  ) =>
  Rule cls name m ->
  t m ()
expectAny = fromExpectSet . makeExpect callStack normalPriority anyMultiplicity

-- | Specifies a default response if a matching action is performed.  This
-- differs from 'expectAny' because other expectations will always override
-- this default.
--
-- In this example, 'whenever' installs a default behavior for @readFile@, but
-- does not override the behavior for @"config.txt"@:
--
-- @
--   'runMockT' '$' do
--     'expectAny' '$' readFile_ "config.txt" '|->' "lang: klingon"
--     'whenever' '$' ReadFile_ anything '|->' "tlhIngan maH!"
--
--     callCodeUnderTest
-- @
whenever ::
  ( HasCallStack,
    Mockable cls,
    Typeable m,
    Monad m,
    KnownSymbol name,
    Expectable t
  ) =>
  Rule cls name m ->
  t m ()
whenever = fromExpectSet . makeExpect callStack lowPriority anyMultiplicity

-- | Creates a sequential expectation.  Other actions can still happen during
-- the sequence, but these specific expectations must be met in this order.
--
-- @
--   'inSequence'
--     [ 'expect' '$' moveForward_ '|->' (),
--       'expect' '$' turnRight_ '|->' (),
--       'expect' '$' moveForward_ '|->' ()
--     ]
-- @
--
-- Beware of using 'inSequence' too often.  It is appropriate when the property
-- you are testing is that the order of effects is correct.  If that's not the
-- purpose of the test, consider adding several independent expectations,
-- instead.  This avoids over-asserting, and keeps your tests less brittle.
inSequence ::
  (Monad m, Expectable t) => (forall u. Expectable u => [u m ()]) -> t m ()
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
--       'expect' '$' 'startCar' '|->' ()
--     ]
-- @
inAnyOrder ::
  (Monad m, Expectable t) => (forall u. Expectable u => [u m ()]) -> t m ()
inAnyOrder es = fromExpectSet (ExpectMulti AnyOrder es)

-- | Monad transformer for running mocks.
newtype MockT m a where
  MockT :: StateT (ExpectSet m ()) m a -> MockT m a
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadFail,
      MonadIO,
      MonadReader r,
      MonadWriter w,
      MonadRWS r w s,
      MonadError e,
      MonadCont,
      MonadBase b,
      MonadCatch,
      MonadMask,
      MonadThrow
    )

instance Expectable MockT where
  fromExpectSet e =
    MockT $ modify (\e' -> simplify (ExpectMulti AnyOrder [e, e']))

instance MonadTrans MockT where
  lift = MockT . lift

instance MonadState s m => MonadState s (MockT m) where
  get = lift get
  put = lift . put

-- | Runs a test in the 'MockT' monad, handling all of the mocks.
runMockT :: Monad m => MockT m a -> m a
runMockT (MockT test) = do
  (a, leftover) <- runStateT test ExpectNothing
  case excess leftover of
    ExpectNothing -> return a
    missing ->
      error $
        "Unmet expectations:\n" ++ formatExpectSet "  " missing

-- | Implements a method in a 'Mockable' monad by delegating to the mock
-- framework.  This is typically used only in generated code.
mockMethod ::
  forall cls name m a.
  (HasCallStack, Mockable cls, KnownSymbol name, Monad m, Typeable m) =>
  Action cls name m a ->
  MockT m a
mockMethod a = withFrozenCallStack $
  MockT $ do
    expected <- get
    let (partials, fulls) = partitionEithers (tryMatch <$> liveSteps expected)
    let orderedPartials = snd <$> sortBy (compare `on` fst) (catMaybes partials)
    let orderedFulls = snd <$> sortBy (flip compare `on` fst) fulls
    case (orderedPartials, orderedFulls) of
      (_, response : _) -> response
      ([], []) -> noMatchError a
      (_, []) -> partialMatchError a orderedPartials
  where
    tryMatch ::
      (Priority, Step, ExpectSet m ()) ->
      Either (Maybe (Int, String)) (Priority, StateT (ExpectSet m ()) m a)
    tryMatch (prio, Step loc step, e)
      | Just (m :-> impl) <- cast step = case matchAction m a of
        NoMatch n -> Left (Just (n, showWithLoc loc (showMatcher (Just a) m)))
        Match Refl | MockT r <- impl a -> Right (prio, put e >> r)
      | otherwise = Left Nothing

-- An error for an action that matches no expectations at all.
noMatchError ::
  (HasCallStack, Mockable cls) =>
  -- | The action that was received.
  Action cls name m a ->
  StateT (ExpectSet m ()) m a
noMatchError a =
  error $
    "Unexpected action: "
      ++ showAction a

-- An error for an action that doesn't match the argument predicates for any
-- of the method's expectations.
partialMatchError ::
  (HasCallStack, Mockable cls) =>
  -- | The action that was received.
  Action cls name m a ->
  -- | Descriptions of the matchers that most closely matched, closest first.
  [String] ->
  StateT (ExpectSet m ()) m a
partialMatchError a partials =
  error $
    "Wrong arguments: "
      ++ showAction a
      ++ "\n\nClosest matches:\n - "
      ++ intercalate "\n - " (take 5 partials)
