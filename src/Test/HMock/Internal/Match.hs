{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The matching engine: delegates an 'Action' to HMock against an explicit
-- t'MockState'.  Front ends wrap this in whatever monad they run tests in.
module Test.HMock.Internal.Match
  ( mockMethodWith,
    mockDefaultlessMethodWith,
  )
where

import Control.Monad (forM, forM_, join, unless, void)
import Control.Monad.Extra (concatMapM)
import Control.Monad.IO.Class (MonadIO)
import Data.Bifunctor (bimap)
import Data.Default (Default (def))
import Data.Either (partitionEithers)
import Data.Function (on)
import Data.List (intercalate, sortBy)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Proxy (Proxy (Proxy))
import Type.Reflection (eqTypeRep, typeRep, (:~~:) (HRefl))
import GHC.Stack (HasCallStack, withFrozenCallStack)
import Test.HMock.Internal.ExpectSet (ExpectSet, liveSteps)
import Test.HMock.Internal.Rule (WholeMethodMatcher (..), showWholeMatcher)
import Test.HMock.Internal.State
  ( MockSTM,
    MockState (..),
    Severity (..),
    allStates,
    askState,
    describeExpectationsWith,
    isInteresting,
    liftSTM,
    reportFault,
    runMockSTM,
  )
import Test.HMock.Internal.Step (SingleRule ((:->)), Step (Step))
import Test.HMock.Internal.Util (InstanceId, Located (Loc), appliesTo, showInstanceId, withLoc)
import Test.HMock.Mockable
  ( MatchResult (..),
    MockableBase (..),
    MockableMethod,
  )
import UnliftIO (TVar, atomically, readTVar, writeTVar)

matchWholeAction ::
  MockableBase cls =>
  WholeMethodMatcher cls name a ->
  Action cls name a ->
  MatchResult
matchWholeAction (JustMatcher m) a = matchAction m a
matchWholeAction (m `SuchThat` p) a = case matchAction m a of
  NoMatch n -> NoMatch n
  Match
    | p a -> Match
    | otherwise -> NoMatch []

-- | Recovers a step's rule at the type of the call being matched, if the step
-- is for the same method: the same interface, name, and return type.
sameOperation ::
  forall k (cls :: k) name r k' (cls' :: k') name' r' m.
  (MockableMethod cls name r, MockableMethod cls' name' r') =>
  Located (SingleRule cls' name' m r') ->
  Maybe (Located (SingleRule cls name m r))
sameOperation rule =
  case ( eqTypeRep (typeRep @cls) (typeRep @cls'),
         eqTypeRep (typeRep @name) (typeRep @name'),
         eqTypeRep (typeRep @r) (typeRep @r')
       ) of
    (Just HRefl, Just HRefl, Just HRefl) -> Just rule
    _ -> Nothing

-- | A live step for the same method that didn't match the call: how it
-- prints, and why it didn't match.
data Partial
  = Partial
      -- | Why the step's instance rules the call out, if it does.
      (Maybe String)
      -- | Argument mismatches, or 'Nothing' if the arguments all matched.  An
      -- empty list means the whole-method predicate failed.
      (Maybe [(Int, String)])
      -- | The step's matcher, as it prints against this call.
      String

-- | How far a partial match is from matching, for ordering the report.
partialDistance :: Partial -> Int
partialDistance (Partial wrongInstance mismatches _) =
  maybe 0 (const 1) wrongInstance + maybe 0 (max 1 . length) mismatches

-- | Implements mock delegation for actions, against an explicit state.
--
-- This is the whole matching engine.  It takes the state rather than reading
-- it from a monad, so that a front end can drive it however it likes.  The
-- t'InstanceId' is the instance the call came from, if the front end has instances;
-- rules for that instance and rules for no particular instance both apply.
mockMethodImpl ::
  forall cls name m r.
  (HasCallStack, MonadIO m, MockableMethod cls name r) =>
  MockState m ->
  Maybe InstanceId ->
  r ->
  Action cls name r ->
  m r
mockMethodImpl state instId surrogate action = join $
  atomically $ runMockSTM state $ do
    currentState <- askState
    let states = allStates currentState
    (partial, full) <- fmap (bimap concat concat . unzip) $
      forM states $ \st -> do
        expectSet <- liftSTM $ readTVar (mockExpectSet st)
        return $
          partitionEithers
            (tryMatch (mockExpectSet st) <$> liveSteps expectSet)
    let orderedPartial = sortBy (compare `on` partialDistance) (catMaybes partial)
    defaults <- concatMapM (liftSTM . readTVar . mockDefaults) states
    unexpected <-
      concatMapM
        (liftSTM . readTVar . mockAllowUnexpected)
        states
    sideEffect <-
      getSideEffect
        <$> concatMapM (liftSTM . readTVar . mockSideEffects) states
    ambigSev <- liftSTM $ readTVar (mockAmbiguitySeverity currentState)
    unintSev <-
      liftSTM $ readTVar (mockUninterestingSeverity currentState)
    unexpSev <- liftSTM $ readTVar (mockUnexpectedSeverity currentState)
    -- byDefault rules supply the response for any matching call that has no
    -- response of its own.  An allowUnexpected response applies only to a
    -- call that is truly unexpected, where it takes precedence over them.
    case ( full,
           orderedPartial,
           allowedUnexpected unexpected,
           findDefault defaults
         ) of
      (opts@((_, choose, response) : rest), _, _, d) -> do
        choose
        return $ do
          unless (null rest) $
            ambiguityError state ambigSev instId action ((\(s, _, _) -> s) <$> opts)
          sideEffect
          fromMaybe d response
      ([], _, Just response, d) -> return (sideEffect >> fromMaybe d response)
      ([], [], _, d) -> do
        interesting <- isInteresting (Proxy :: Proxy cls) (Proxy :: Proxy name)
        case (interesting, unintSev) of
          (True, _) -> return (noMatchError state unexpSev instId action >> d)
          (False, Error) -> return (noMatchError state unexpSev instId action >> d)
          _ -> return (uninterestingError unintSev instId action >> d)
      ([], _, _, d) ->
        return (partialMatchError state unexpSev instId action orderedPartial >> d)
  where
    respond :: (Action cls name r -> m r) -> m r
    respond impl = impl action

    tryMatch ::
      TVar (ExpectSet (Step m)) ->
      (Step m, ExpectSet (Step m)) ->
      Either (Maybe Partial) (String, MockSTM m (), Maybe (m r))
    tryMatch tvar (Step stepInst expected, e)
      | Just lrule@(Loc _ (m :-> impl)) <- sameOperation expected =
        let shown = withLoc (showWholeMatcher (Just action) m <$ lrule)
            wrongInstance
              | stepInst `appliesTo` instId = Nothing
              | otherwise =
                Just $
                  "Expected" ++ showInstanceId stepInst
                    ++ ", but called" ++ showInstanceId instId
         in case (wrongInstance, matchWholeAction m action) of
              (Nothing, Match) ->
                Right (shown, liftSTM $ writeTVar tvar e, respond <$> impl)
              (_, Match) -> Left (Just (Partial wrongInstance Nothing shown))
              (_, NoMatch n) -> Left (Just (Partial wrongInstance (Just n) shown))
      | otherwise = Left Nothing

    allowedUnexpected :: [Step m] -> Maybe (Maybe (m r))
    allowedUnexpected [] = Nothing
    allowedUnexpected (Step stepInst unexpected : steps)
      | stepInst `appliesTo` instId,
        Just (Loc _ (m :-> impl)) <- sameOperation unexpected,
        Match <- matchWholeAction m action =
        Just (respond <$> impl)
      | otherwise = allowedUnexpected steps

    findDefault :: [Step m] -> m r
    findDefault [] = return surrogate
    findDefault (Step stepInst expected : steps)
      | stepInst `appliesTo` instId,
        Just (Loc _ (m :-> impl)) <- sameOperation expected,
        Match <- matchWholeAction m action =
        maybe (findDefault steps) respond impl
      | otherwise = findDefault steps

    getSideEffect :: [Step m] -> m ()
    getSideEffect effects =
      forM_ effects $ \(Step stepInst expected) -> case sameOperation expected of
        Just (Loc _ (m :-> Just impl))
          | stepInst `appliesTo` instId,
            Match <- matchWholeAction m action ->
            void (impl action)
        _ -> return ()

-- | Delegates an action to the mock framework, against an explicit state.  If
-- the call is unexpected, an exception is thrown.  An expected call without a
-- specified response returns the default value of its return type.  The
-- t'InstanceId' is the instance the call came from, if any.
mockMethodWith ::
  ( HasCallStack,
    MonadIO m,
    MockableMethod cls name r,
    Default r
  ) =>
  MockState m ->
  Maybe InstanceId ->
  Action cls name r ->
  m r
mockMethodWith state instId action =
  withFrozenCallStack $ mockMethodImpl state instId def action

-- | Like 'mockMethodWith', but an expected call without a specified response
-- returns 'undefined'.  This is for return types with no 'Default'.
mockDefaultlessMethodWith ::
  ( HasCallStack,
    MonadIO m,
    MockableMethod cls name r
  ) =>
  MockState m ->
  Maybe InstanceId ->
  Action cls name r ->
  m r
mockDefaultlessMethodWith state instId action =
  withFrozenCallStack $ mockMethodImpl state instId undefined action

-- | An error for an action that matches no expectations at all.  This is only
-- used if severity is Ignore or Warning.
uninterestingError ::
  (HasCallStack, MockableBase cls, MonadIO m) =>
  Severity ->
  Maybe InstanceId ->
  Action cls name r ->
  m ()
uninterestingError severity instId a =
  reportFault severity $
    "Uninteresting action: " ++ showAction a ++ showInstanceId instId

-- | An error for an action that matches no expectations at all.
noMatchError ::
  (HasCallStack, MockableBase cls, MonadIO m) =>
  MockState m ->
  Severity ->
  Maybe InstanceId ->
  Action cls name r ->
  m ()
noMatchError state severity instId a = do
  fullExpectations <- describeExpectationsWith state
  reportFault severity $
    "Unexpected action: " ++ showAction a ++ showInstanceId instId
      ++ "\n\nFull expectations:\n"
      ++ fullExpectations

-- | An error for an action that has expectations for its method, none of
-- which match it: the arguments differ, or the expectation is for another
-- instance.  The partial matches are ordered closest first.
partialMatchError ::
  (HasCallStack, MockableBase cls, MonadIO m) =>
  MockState m ->
  Severity ->
  Maybe InstanceId ->
  Action cls name r ->
  [Partial] ->
  m ()
partialMatchError state severity instId a partials = do
  fullExpectations <- describeExpectationsWith state
  reportFault severity $
    heading
      ++ showAction a
      ++ showInstanceId instId
      ++ "\n\nClosest matches:\n - "
      ++ intercalate "\n - " (map formatPartial $ take 5 partials)
      ++ "\n\nFull expectations:\n"
      ++ fullExpectations
  where
    heading = case partials of
      Partial (Just _) Nothing _ : _ -> "Wrong instance: "
      _ -> "Wrong arguments: "

    formatPartial :: Partial -> String
    formatPartial (Partial wrongInstance mismatches matcher) =
      matcher ++ concatMap ("\n   * " ++) (maybe [] pure wrongInstance ++ argReasons)
      where
        argReasons = case mismatches of
          Nothing -> []
          Just [] -> ["Failed whole-method matcher"]
          Just ms -> ["Arg #" ++ show i ++ ": " ++ mm | (i, mm) <- ms]

-- | An error for an 'Action' that matches more than one 'Matcher'.  This only
-- triggers an error if ambiguity checks are on.
ambiguityError ::
  (HasCallStack, MockableBase cls, MonadIO m) =>
  MockState m ->
  Severity ->
  Maybe InstanceId ->
  Action cls name r ->
  [String] ->
  m ()
ambiguityError state severity instId a choices = do
  fullExpectations <- describeExpectationsWith state
  reportFault severity $
    "Ambiguous action matched multiple expectations: "
      ++ showAction a
      ++ showInstanceId instId
      ++ "\n\nMatches:\n - "
      ++ intercalate "\n - " choices
      ++ "\n\nFull expectations:\n"
      ++ fullExpectations
