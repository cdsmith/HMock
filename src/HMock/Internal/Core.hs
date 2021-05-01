{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module HMock.Internal.Core where

import Control.Arrow (Arrow (second))
import Control.Monad.State (MonadState (get, put), StateT (..), modify)
import Data.Constraint (Constraint)
import Data.Dynamic (Dynamic, Typeable, fromDynamic, toDyn)
import Data.Either (partitionEithers)
import Data.List (intercalate, sort)
import Data.Maybe (mapMaybe)
import Data.Type.Equality (type (:~:) (..))
import HMock.Internal.Cardinality
  ( Cardinality (..),
    anyCardinality,
    decCardinality,
    once,
  )

-- | A single step of an expectation.
--
-- The 'Dynamic' is always a @'WithResult' ctx m@ for some choice of @ctx@ and
-- @m@.
data Step where
  Step :: String -> Dynamic -> Step

-- | A set of expected actions and their responses.  An entire test with mocks
-- is expected to run in a single base 'Monad', which is the type parameter
-- here.
data Expected (m :: * -> *) where
  ExpectNothing :: Expected m
  Expect :: Cardinality -> Step -> Expected m
  AllOf :: [Expected m] -> Expected m
  Sequence :: [Expected m] -> Expected m

-- | Converts a set of expectations into a string that summarizes them, with
-- the given prefix (used to indent).
formatExpected :: String -> Expected m -> String
formatExpected prefix ExpectNothing = prefix ++ "nothing"
formatExpected prefix (Expect card (Step s _))
  | card == once = prefix ++ s
  | otherwise = prefix ++ s ++ " (" ++ show card ++ ")"
formatExpected prefix (AllOf xs) =
  prefix ++ "all of (in any order):\n"
    ++ unlines (map (formatExpected (prefix ++ "  ")) xs)
formatExpected prefix (Sequence xs) =
  prefix ++ "in sequence:\n"
    ++ unlines (map (formatExpected (prefix ++ "  ")) xs)

-- | Get a list of steps that can match actions right now, together with the
-- remaining expectations if each one were to match.
liveSteps :: Expected m -> [(Step, Expected m)]
liveSteps = map (second simplify) . go
  where
    go ExpectNothing = []
    go (Expect card step) = case decCardinality card of
      Nothing -> [(step, ExpectNothing)]
      Just card' -> [(step, Expect card' step)]
    go (AllOf es) =
      [(a, AllOf (e' : es')) | (e, es') <- choices es, (a, e') <- go e]
      where
        choices [] = []
        choices (x : xs) =
          (x, xs) : (fmap (x :) <$> choices xs)
    go (Sequence es) =
      [(a, Sequence (e' : es')) | e : es' <- [es], (a, e') <- go e]

-- | Simplifies a set of expectations.  This removes unnecessary occurrences of
-- 'ExpectNothing' and collapses nested lists with the same ordering
-- constraints.
simplify :: Expected m -> Expected m
simplify e = case e of
  (AllOf xs) -> simplifyMulti False xs
  (Sequence xs) -> simplifyMulti True xs
  _ -> e
  where
    simplifyMulti :: Bool -> [Expected m] -> Expected m
    simplifyMulti order =
      construct order . concatMap (expand order . simplify)

    expand :: Bool -> Expected m -> [Expected m]
    expand _ ExpectNothing = []
    expand order (Sequence xs) | order = xs
    expand order (AllOf xs) | not order = xs
    expand _ other = [other]

    construct :: Bool -> [Expected m] -> Expected m
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
    go (Expect (Interval lo _) step)
      | lo == 0 = ExpectNothing
      | otherwise = Expect (Interval lo Nothing) step
    go (AllOf xs) = AllOf (map go xs)
    go (Sequence xs) = Sequence (map go xs)

-- | The result of matching a @'Matcher' a@ with an @'Action' b@.
data MatchResult a b where
  -- | The 'Matcher' was for a different method.
  NoMatch :: MatchResult a b
  -- | The 'Matcher' was for the right method, but this number of arguments
  -- don't match.  'Refl' witnesses equality of return types.
  PartialMatch :: a :~: a -> Int -> MatchResult a b
  -- | This is a match. 'Refl' witnesses equality of return types.
  FullMatch :: a :~: b -> MatchResult a b

-- | A class for 'Monad' subclasses whose methods can be mocked.  You usually
-- want to generate this instance using 'HMock.TH.makeMockable' or
-- 'HMock.TH.deriveMockable', because it's just a lot of boilerplate.
class Typeable ctx => Mockable (ctx :: (* -> *) -> Constraint) where
  -- An action that is performed.  This data type will have one constructor for
  -- each method.
  data Action ctx :: * -> *

  -- | A specification for matching actions.  The actual arguments should be
  -- replaced with predicates.
  data Matcher ctx :: * -> *

  -- Gets a text description of an 'Action', for use in error messages.
  showAction :: Action ctx a -> String

  -- Gets a text description of a 'Matcher', for use in error messages.
  showMatcher :: Maybe (Action ctx a) -> Matcher ctx b -> String

  -- Attempts to match an 'Action' with a 'Matcher'.
  match :: Matcher ctx a -> Action ctx b -> MatchResult a b

-- | A class for 'Monad' subclasses whose methods can be mocked and compared
-- for exact equality.  Only those classes whose methods have nice enough
-- arguments (which can be printed and compared for equality) may be instances
-- of this subclass.
class Mockable ctx => ExactMockable ctx where
  -- Converts an 'Action' into a 'Matcher' that will only match those exact
  -- parameter values.  This is sometimes more convenient than writing
  -- 'HMock.eq_' everywhere.
  exactly :: Action ctx a -> Matcher ctx a

-- | Monad transformer for running mocks.
newtype MockT m a where
  MockT :: StateT (Expected m) m a -> MockT m a
  deriving (Functor, Applicative, Monad)

-- | Runs a test in the 'MockT' monad, handling all of the mocks.
runMockT :: Monad m => MockT m a -> m a
runMockT (MockT test) = do
  (a, leftover) <- runStateT test ExpectNothing
  case excess leftover of
    ExpectNothing -> return a
    missing ->
      error $
        "Missing expectations:\n" ++ formatExpected "  " missing

-- | A pair of a 'Matcher' and a response for when it matches.  The matching
-- 'Action' is passed to the response, and is guaranteed to be a match, so it's
-- okay to just pattern match on the correct method.
data WithResult (ctx :: (* -> *) -> Constraint) (m :: * -> *) where
  -- | Matches an 'Action' and performs a response in the 'MockT' monad.  This
  -- is a vary flexible response, which can look at arguments, do things in the
  -- base monad, set up more expectations, etc.
  (:=>) :: Matcher ctx a -> (Action ctx a -> MockT m a) -> WithResult ctx m

-- | Matches an 'Action' and returns a constant response.  This is more
-- convenient than '(:=>)' in the common case where you just want to return a
-- known result.
(|=>) ::
  (Mockable ctx, Monad m) =>
  Matcher ctx a ->
  a ->
  WithResult ctx m
m |=> r = m :=> const (return r)

-- | Matches an exact 'Action' and returns a constant response.  This is the
-- simplest way to write an expectation, and is more readable than '(:=>)' or
-- '(|=>)' when you know the exact arguments that must be passed to the
-- method.  However, it can lead to over-assertion if used too often, which can
-- make your tests brittle and less useful.
(|->) ::
  (ExactMockable ctx, Monad m) =>
  Action ctx a ->
  a ->
  WithResult ctx m
a |-> b = exactly a |=> b

-- | Implements a method in a 'Mockable' monad by delegating to the mock
-- framework.  This is typically used only in generated code.
mockMethod ::
  forall ctx m a.
  (Mockable ctx, Monad m, Typeable m) =>
  Action ctx a ->
  MockT m a
mockMethod a = MockT $ do
  expected <- get
  case partitionEithers (mapMaybe tryMatch (liveSteps expected)) of
    ([], []) -> noMatchError a
    (partials, []) -> partialMatchError a (map snd (sort partials))
    (_, [(_, response)]) -> response
    (_, successes) -> ambiguousMatchError a (map fst successes)
  where
    tryMatch ::
      (Step, Expected m) ->
      Maybe
        ( Either
            (Int, String)
            (String, StateT (Expected m) m a)
        )
    tryMatch (Step _ step, e)
      | Just (m :=> impl) <-
          fromDynamic step :: Maybe (WithResult ctx m) =
        case match m a of
          NoMatch -> Nothing
          PartialMatch Refl n -> Just (Left (n, showMatcher (Just a) m))
          FullMatch Refl
            | MockT r <- impl a -> Just (Right (showMatcher (Just a) m, put e >> r))
    tryMatch _ = Nothing

-- An error for an action that matches no expectations at all.
noMatchError ::
  Mockable ctx =>
  -- | The action that was received.
  Action ctx a ->
  m a
noMatchError a =
  error $
    "Unexpected action: "
      ++ showAction a

-- An error for an action that doesn't match the argument predicates for any
-- of the method's expectations.
partialMatchError ::
  Mockable ctx =>
  -- | The action that was received.
  Action ctx a ->
  -- | Descriptions of the matchers that most closely matched, closest first.
  [String] ->
  m a
partialMatchError a partials =
  error $
    "Wrong arguments: "
      ++ showAction a
      ++ "\n\nClosest matches:\n - "
      ++ intercalate "\n - " partials

-- An error for an action that matched more than one expectation.
ambiguousMatchError ::
  Mockable ctx =>
  -- | The action that was received.
  Action ctx a ->
  -- | Descriptions of the matchers that matched the action.
  [String] ->
  m a
ambiguousMatchError a matches =
  error $
    "Ambiguous matches for action: "
      ++ showAction a
      ++ "\nPossible matches:\n - "
      ++ intercalate "\n - " matches

-- Adds expectations to a test case running in 'MockT'.
mock :: Monad m => Expected m -> MockT m ()
mock newExpected = MockT $ modify (\e -> simplify (AllOf [e, newExpected]))

-- Creates an expectation that an action is performed some number of times.
expectN ::
  forall m ctx.
  (Typeable m, Mockable ctx) =>
  -- | The number of times the action should be performed.
  Cardinality ->
  -- | The action and its response.
  WithResult ctx m ->
  Expected m
expectN card wr@(m :=> (_ :: Action ctx a -> MockT m a)) =
  Expect card (Step (showMatcher Nothing m) (toDyn wr))

-- Creates an expectation that an action is performed once.  This is equivalent
-- to @'expectN' 'once'@, but shorter.
expect ::
  forall m ctx.
  (Typeable m, Mockable ctx) =>
  WithResult ctx m ->
  Expected m
expect = expectN once

-- Creates an expectation that an action is performed zero or more times.  This
-- is equivalent to @'expectN' 'anyCardinality'@, but shorter.
whenever ::
  forall m ctx.
  (Typeable m, Mockable ctx) =>
  WithResult ctx m ->
  Expected m
whenever = expectN anyCardinality

-- Creates a sequential expectation.  Other actions can still happen during the
-- sequence, but these specific expectations must be met in this order.
--
-- Beware of using 'inSequence' too often.  It is appropriate when the property
-- you are testing is that the order of effects is correct.  If that's not the
-- purpose of the test, consider adding several independent expectations,
-- instead.  This avoids over-asserting, and keeps your tests less brittle.
inSequence :: [Expected m] -> Expected m
inSequence = Sequence
