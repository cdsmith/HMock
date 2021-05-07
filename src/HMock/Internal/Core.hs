{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module HMock.Internal.Core where

import Control.Monad.Base (MonadBase)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Cont (MonadCont)
import Control.Monad.Except (MonadError)
import Control.Monad.RWS (MonadRWS)
import Control.Monad.Reader (MonadReader)
import Control.Monad.State (MonadState (get, put), StateT (..), modify)
import Control.Monad.Trans (MonadIO, MonadTrans (..))
import Control.Monad.Writer (MonadWriter)
import Data.Constraint (Constraint)
import Data.Dynamic (Dynamic, Typeable, fromDynamic, toDyn)
import Data.Either (partitionEithers)
import Data.Function (on)
import Data.List (intercalate, sort, sortBy)
import Data.Maybe (mapMaybe)
import Data.Type.Equality (type (:~:) (..))
import GHC.Stack (CallStack, HasCallStack, callStack, withFrozenCallStack)
import GHC.TypeLits (KnownSymbol, Symbol)
import HMock.Internal.Cardinality
  ( Cardinality (..),
    anyCardinality,
    decCardinality,
    once,
  )
import HMock.Internal.Util (Loc, getSrcLoc, showWithLoc)

newtype Priority = Priority Int deriving (Show, Eq, Ord)

lowPriority :: Priority
lowPriority = Priority 0

normalPriority :: Priority
normalPriority = Priority 1

-- | A single step of an expectation.
--
-- The 'Dynamic' is always a @'Rule' ctx m@ for some choice of @ctx@ and
-- @m@.
data Step where
  Step :: Loc -> String -> Dynamic -> Step

-- | A set of expected actions and their responses.  An entire test with mocks
-- is expected to run in a single base 'Monad', which is the type parameter
-- here.
data Expected (m :: * -> *) where
  ExpectNothing :: Expected m
  Expect :: Priority -> Cardinality -> Step -> Expected m
  AllOf :: [Expected m] -> Expected m
  Sequence :: [Expected m] -> Expected m

-- | Converts a set of expectations into a string that summarizes them, with
-- the given prefix (used to indent).
formatExpected :: String -> Expected m -> String
formatExpected prefix ExpectNothing = prefix ++ "nothing"
formatExpected prefix (Expect prio card (Step loc s _)) =
  showWithLoc loc (prefix ++ s) ++ modifierDesc
  where
    modifiers = prioModifier ++ cardModifier
    prioModifier
      | prio == lowPriority = ["low priority"]
      | otherwise = []
    cardModifier
      | card == once = []
      | otherwise = [show card]
    modifierDesc
      | null modifiers = ""
      | otherwise = " (" ++ intercalate ", " modifiers ++ ")"
formatExpected prefix (AllOf xs) =
  prefix ++ "all of (in any order):\n"
    ++ unlines (map (formatExpected (prefix ++ "  ")) xs)
formatExpected prefix (Sequence xs) =
  prefix ++ "in sequence:\n"
    ++ unlines (map (formatExpected (prefix ++ "  ")) xs)

-- | Get a list of steps that can match actions right now, together with the
-- remaining expectations if each one were to match.
liveSteps :: Expected m -> [(Priority, Step, Expected m)]
liveSteps = map (\(p, s, e) -> (p, s, simplify e)) . go
  where
    go ExpectNothing = []
    go (Expect prio card step) = case decCardinality card of
      Nothing -> [(prio, step, ExpectNothing)]
      Just card' -> [(prio, step, Expect prio card' step)]
    go (AllOf es) =
      [(p, a, AllOf (e' : es')) | (e, es') <- choices es, (p, a, e') <- go e]
      where
        choices [] = []
        choices (x : xs) = (x, xs) : (fmap (x :) <$> choices xs)
    go (Sequence es) =
      [(p, a, Sequence (e' : es')) | (e, es') <- choices es, (p, a, e') <- go e]
      where
        choices [] = []
        choices (x : xs)
          | ExpectNothing <- excess x = (x, xs) : choices xs
          | otherwise = [(x, xs)]

-- | Simplifies a set of expectations.  This removes unnecessary occurrences of
-- 'ExpectNothing' and collapses nested lists with the same ordering
-- constraints.
simplify :: Expected m -> Expected m
simplify e = case e of
  (AllOf xs) -> simplifyMulti False xs
  (Sequence xs) -> simplifyMulti True xs
  _ -> e
  where
    simplifyMulti order =
      construct order . concatMap (expand order . simplify)

    expand _ ExpectNothing = []
    expand order (Sequence xs) | order = xs
    expand order (AllOf xs) | not order = xs
    expand _ other = [other]

    construct _ [] = ExpectNothing
    construct _ [x] = x
    construct order xs
      | order = Sequence xs
      | otherwise = AllOf xs

-- | Reduces a set of expectations to the minimum steps that would be required
-- to satisfy the entire set.  This weeds out unnecessary information before
-- reporting that there were unmet expectations at the end of the test.
excess :: Expected m -> Expected m
excess = simplify . go
  where
    go ExpectNothing = ExpectNothing
    go e@(Expect _ (Interval lo _) _)
      | lo == 0 = ExpectNothing
      | otherwise = e
    go (AllOf xs) = AllOf (map go xs)
    go (Sequence xs) = Sequence (map go xs)

-- | The result of matching a @'Matcher' a@ with an @'Action' b@.  Because the
-- types should already guarantee that the methods match, all that's left is to
-- match arguments.
data MatchResult a b where
  -- | No match.  The int is the number of arguments that don't match.
  NoMatch :: Int -> MatchResult a b
  -- | Match. 'Refl' witnesses equality of return types.
  Match :: a :~: b -> MatchResult a b

-- | A class for 'Monad' subclasses whose methods can be mocked.  You usually
-- want to generate this instance using 'HMock.TH.makeMockable' or
-- 'HMock.TH.deriveMockable', because it's just a lot of boilerplate.
class Typeable ctx => Mockable (ctx :: (* -> *) -> Constraint) where
  -- | An action that is performed.  This data type will have one constructor
  -- for each method.
  data Action ctx :: Symbol -> (* -> *) -> * -> *

  -- | A specification for matching actions.  The actual arguments should be
  -- replaced with predicates.
  data Matcher ctx :: Symbol -> (* -> *) -> * -> *

  -- | Gets a text description of an 'Action', for use in error messages.
  showAction :: Action ctx name m a -> String

  -- | Gets a text description of a 'Matcher', for use in error messages.
  showMatcher :: Maybe (Action ctx name m a) -> Matcher ctx name m b -> String

  -- | Attempts to match an 'Action' with a 'Matcher'.
  match :: Matcher ctx name m a -> Action ctx name m b -> MatchResult a b

-- | Monad transformer for running mocks.
newtype MockT m a where
  MockT :: StateT (Expected m) m a -> MockT m a
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
        "Unmet expectations:\n" ++ formatExpected "  " missing

-- | A pair of a 'Matcher' and a response for when it matches.  The matching
-- 'Action' is passed to the response, and is guaranteed to be a match, so it's
-- okay to just pattern match on the correct method.
data Rule (ctx :: (* -> *) -> Constraint) (name :: Symbol) (m :: * -> *) where
  -- | Matches an 'Action' and performs a response in the 'MockT' monad.  This
  -- is a vary flexible response, which can look at arguments, do things in the
  -- base monad, set up more expectations, etc.
  (:->) ::
    Matcher ctx name m a ->
    (Action ctx name m a -> MockT m a) ->
    Rule ctx name m

-- | Matches an 'Action' and returns a constant response.  This is more
-- convenient than '(:->)' in the common case where you just want to return a
-- known result.
(|->) :: (Mockable ctx, Monad m) => Matcher ctx name m a -> a -> Rule ctx name m
m |-> r = m :-> const (return r)

-- | Implements a method in a 'Mockable' monad by delegating to the mock
-- framework.  This is typically used only in generated code.
mockMethod ::
  forall ctx name m a.
  (HasCallStack, Mockable ctx, KnownSymbol name, Monad m, Typeable m) =>
  Action ctx name m a ->
  MockT m a
mockMethod a = withFrozenCallStack $
  MockT $ do
    expected <- get
    let (partials, fulls) =
          partitionEithers (mapMaybe tryMatch (liveSteps expected))
    let maxPrioFulls =
          dropLowPrio (sortBy (flip compare `on` \(p, _, _, _) -> p) fulls)
    case (partials, maxPrioFulls) of
      ([], []) -> noMatchError a
      (_, []) ->
        partialMatchError
          a
          (map (\(_, loc, m) -> showWithLoc loc m) (sort partials))
      (_, [(_, _, _, response)]) -> response
      (_, successes) ->
        ambiguousMatchError
          a
          (map (\(_, loc, m, _) -> showWithLoc loc m) successes)
  where
    tryMatch ::
      (Priority, Step, Expected m) ->
      Maybe
        ( Either
            (Int, Loc, String)
            (Priority, Loc, String, StateT (Expected m) m a)
        )
    tryMatch (prio, Step loc _ step, e)
      | Just (m :-> impl) <-
          fromDynamic step :: Maybe (Rule ctx name m) =
        case match m a of
          NoMatch n -> Just (Left (n, loc, showMatcher (Just a) m))
          Match Refl
            | MockT r <- impl a ->
              Just (Right (prio, loc, showMatcher (Just a) m, put e >> r))
    tryMatch _ = Nothing

    dropLowPrio [] = []
    dropLowPrio ((p, l, c, r) : rest) =
      (p, l, c, r) : takeWhile (\(p', _, _, _) -> p' == p) rest

-- An error for an action that matches no expectations at all.
noMatchError ::
  (HasCallStack, Mockable ctx) =>
  -- | The action that was received.
  Action ctx name m a ->
  StateT (Expected m) m a
noMatchError a =
  error $
    "Unexpected action: "
      ++ showAction a

-- An error for an action that doesn't match the argument predicates for any
-- of the method's expectations.
partialMatchError ::
  (HasCallStack, Mockable ctx) =>
  -- | The action that was received.
  Action ctx name m a ->
  -- | Descriptions of the matchers that most closely matched, closest first.
  [String] ->
  StateT (Expected m) m a
partialMatchError a partials =
  error $
    "Wrong arguments: "
      ++ showAction a
      ++ "\n\nClosest matches:\n - "
      ++ intercalate "\n - " (take 5 partials)

-- An error for an action that matched more than one expectation.
ambiguousMatchError ::
  (HasCallStack, Mockable ctx) =>
  -- | The action that was received.
  Action ctx name m a ->
  -- | Descriptions of the matchers that matched the action.
  [String] ->
  StateT (Expected m) m a
ambiguousMatchError a matches =
  error $
    "Ambiguous matches for action: "
      ++ showAction a
      ++ "\nPossible matches:\n - "
      ++ intercalate "\n - " matches

-- | Adds expectations to a test case running in 'MockT'.
mock :: Monad m => Expected m -> MockT m ()
mock newExpected = MockT $ modify (\e -> simplify (AllOf [e, newExpected]))

makeExpect ::
  (Mockable ctx, Typeable m, KnownSymbol name) =>
  CallStack ->
  Priority ->
  Cardinality ->
  Rule ctx name m ->
  Expected m
makeExpect cs prio card wr@(m :-> (_ :: Action ctx name m a -> MockT m a)) =
  Expect prio card (Step (getSrcLoc cs) (showMatcher Nothing m) (toDyn wr))

-- | Creates an expectation that an action is performed once.  This is
-- equivalent to @'expectN' 'once'@, but shorter.
expect ::
  (HasCallStack, Mockable ctx, Typeable m, KnownSymbol name) =>
  Rule ctx name m ->
  Expected m
expect = makeExpect callStack normalPriority once

-- | Creates an expectation that an action is performed some number of times.
expectN ::
  (HasCallStack, Mockable ctx, Typeable m, KnownSymbol name) =>
  -- | The number of times the action should be performed.
  Cardinality ->
  -- | The action and its response.
  Rule ctx name m ->
  Expected m
expectN = makeExpect callStack normalPriority

-- | Creates an expectation that an action is performed any number of times.
-- This is equivalent to @'expectN' 'anyCardinality'@, but shorter.
expectAny ::
  (HasCallStack, Mockable ctx, Typeable m, KnownSymbol name) =>
  Rule ctx name m ->
  Expected m
expectAny = makeExpect callStack normalPriority anyCardinality

-- | Specifies a default response if a matching action is performed.  This
-- differs from 'expectAny' because other expectations will always override
-- this default.
whenever ::
  (HasCallStack, Mockable ctx, Typeable m, KnownSymbol name) =>
  Rule ctx name m ->
  Expected m
whenever = makeExpect callStack lowPriority anyCardinality

-- | Creates a sequential expectation.  Other actions can still happen during
-- the sequence, but these specific expectations must be met in this order.
--
-- Beware of using 'inSequence' too often.  It is appropriate when the property
-- you are testing is that the order of effects is correct.  If that's not the
-- purpose of the test, consider adding several independent expectations,
-- instead.  This avoids over-asserting, and keeps your tests less brittle.
inSequence :: [Expected m] -> Expected m
inSequence = Sequence

-- | Combines multiple expectations, which can occur in any order.  Most of the
-- time, you can achieve the same thing with multiple uses of 'mock', but this
-- can be combined with 'inSequence' to describe more complex ordering
-- constraints, such as:
--
-- @
--   mock $ inSequence
--     [ inAnyOrder
--         [ expect $ adjustMirrors :-> (),
--           expect $ fastenSeatBelt :-> ()
--         ],
--       expect $ startCard :-> ()
--     ]
-- @
inAnyOrder :: [Expected m] -> Expected m
inAnyOrder = AllOf
