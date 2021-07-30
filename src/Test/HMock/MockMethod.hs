{-# LANGUAGE ScopedTypeVariables #-}

-- | Functions to delegate 'Action's to HMock to match expectations.  There is
-- one delegation function that works if the return type has a 'Default'
-- instance, and another that doesn't require the 'Default' instance, but causes
-- the method to return 'undefined' by default.
module Test.HMock.MockMethod
  ( mockMethod,
    mockDefaultlessMethod,
  )
where

import Control.Concurrent.STM (TVar, readTVar, writeTVar)
import Control.Monad (forM, forM_, join, unless, void)
import Control.Monad.Extra (concatMapM)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ask)
import Data.Bifunctor (bimap)
import Data.Default (Default (def))
import Data.Either (partitionEithers)
import Data.Function (on)
import Data.Functor (($>))
import Data.List (intercalate, sortBy)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Proxy (Proxy (Proxy))
import Data.Typeable (cast)
import GHC.Stack (HasCallStack, withFrozenCallStack)
import Test.HMock.ExpectContext (MockableMethod)
import Test.HMock.Internal.ExpectSet (ExpectSet, liveSteps)
import Test.HMock.Internal.Rule (WholeMethodMatcher (..), showWholeMatcher)
import Test.HMock.Internal.State
  ( MockContext (..),
    MockSetup (..),
    MockState (..),
    MockT,
    Severity (..),
    allStates,
    initClassIfNeeded,
    isInteresting,
    mockSetupSTM,
    reportFault,
  )
import Test.HMock.Internal.Step (SingleRule ((:->)), Step (Step))
import Test.HMock.Internal.Util (Located (Loc), withLoc)
import Test.HMock.MockT (describeExpectations)
import Test.HMock.Mockable
  ( MatchResult (..),
    Mockable (..),
    MockableBase (..),
  )

matchWholeAction ::
  MockableBase cls =>
  WholeMethodMatcher cls name m a ->
  Action cls name m a ->
  MatchResult
matchWholeAction (JustMatcher m) a = matchAction m a
matchWholeAction (m `SuchThat` p) a = case matchAction m a of
  NoMatch n -> NoMatch n
  Match
    | p a -> Match
    | otherwise -> NoMatch []

-- | Implements mock delegation for actions.
mockMethodImpl ::
  forall cls name m r.
  (HasCallStack, MonadIO m, MockableMethod cls name m r) =>
  r ->
  Action cls name m r ->
  MockT m r
mockMethodImpl surrogate action = join $
  fromMockSetup $ do
    initClassIfNeeded (Proxy :: Proxy cls)
    states <- allStates <$> MockSetup ask
    (partial, full) <- fmap (bimap concat concat . unzip) $
      forM states $ \state -> do
        expectSet <- mockSetupSTM $ readTVar (mockExpectSet state)
        return $
          partitionEithers
            (tryMatch (mockExpectSet state) <$> liveSteps expectSet)
    let orderedPartial = sortBy (compare `on` (length . fst)) (catMaybes partial)
    defaults <- concatMapM (mockSetupSTM . readTVar . mockDefaults) states
    unexpected <-
      concatMapM
        (mockSetupSTM . readTVar . mockAllowUnexpected)
        states
    sideEffect <-
      getSideEffect
        <$> concatMapM (mockSetupSTM . readTVar . mockSideEffects) states
    ambigSev <- mockSetupSTM $ readTVar . mockAmbiguitySeverity . head $ states
    unintSev <-
      mockSetupSTM $ readTVar . mockUninterestingSeverity . head $ states
    unexpSev <- mockSetupSTM $ readTVar . mockUnexpectedSeverity . head $ states
    case ( full,
           orderedPartial,
           allowedUnexpected unexpected,
           findDefault defaults
         ) of
      (opts@((_, choose, response) : rest), _, _, d) -> do
        choose
        return $ do
          unless (null rest) $
            ambiguityError ambigSev action ((\(s, _, _) -> s) <$> opts)
          sideEffect
          fromMaybe d response
      ([], _, Just response, d) -> return (sideEffect >> fromMaybe d response)
      ([], [], _, d) -> do
        interesting <- isInteresting (Proxy :: Proxy cls) (Proxy :: Proxy name)
        case (interesting, unintSev) of
          (True, _) -> return (noMatchError unexpSev action >> d)
          (False, Error) -> return (noMatchError unexpSev action >> d)
          _ -> return (uninterestingError unintSev action >> d)
      ([], _, _, d) ->
        return (partialMatchError unexpSev action orderedPartial >> d)
  where
    tryMatch ::
      TVar (ExpectSet (Step m)) ->
      (Step m, ExpectSet (Step m)) ->
      Either
        (Maybe ([(Int, String)], String))
        (String, MockSetup m (), Maybe (MockT m r))
    tryMatch tvar (Step expected, e)
      | Just lrule@(Loc _ (m :-> impl)) <- cast expected =
        case matchWholeAction m action of
          NoMatch n ->
            Left (Just (n, withLoc (showWholeMatcher (Just action) m <$ lrule)))
          Match ->
            Right
              ( withLoc (lrule $> showWholeMatcher (Just action) m),
                mockSetupSTM $ writeTVar tvar e,
                ($ action) <$> impl
              )
      | otherwise = Left Nothing

    allowedUnexpected :: [Step m] -> Maybe (Maybe (MockT m r))
    allowedUnexpected [] = Nothing
    allowedUnexpected (Step unexpected : steps)
      | Just (Loc _ (m :-> impl)) <- cast unexpected,
        Match <- matchWholeAction m action =
        Just (($ action) <$> impl)
      | otherwise = allowedUnexpected steps

    findDefault :: [Step m] -> MockT m r
    findDefault [] = return surrogate
    findDefault (Step expected : steps)
      | Just (Loc _ (m :-> impl)) <- cast expected,
        Match <- matchWholeAction m action =
        maybe (findDefault steps) ($ action) impl
      | otherwise = findDefault steps

    getSideEffect :: [Step m] -> MockT m ()
    getSideEffect effects =
      forM_ effects $ \(Step expected) -> case cast expected of
        Just (Loc _ (m :-> Just impl))
          | Match <- matchWholeAction m action -> void (impl action)
        _ -> return ()

-- | Implements a method in a 'Mockable' monad by delegating to the mock
-- framework.  If the method is called unexpectedly, an exception will be
-- thrown.  However, an expected invocation without a specified response will
-- return the default value.
mockMethod ::
  ( HasCallStack,
    MonadIO m,
    MockableMethod cls name m r,
    Default r
  ) =>
  Action cls name m r ->
  MockT m r
mockMethod action =
  withFrozenCallStack $ mockMethodImpl def action

-- | Implements a method in a 'Mockable' monad by delegating to the mock
-- framework.  If the method is called unexpectedly, an exception will be
-- thrown.  However, an expected invocation without a specified response will
-- return undefined.  This can be used in place of 'mockMethod' when the return
-- type has no default.
mockDefaultlessMethod ::
  ( HasCallStack,
    MonadIO m,
    MockableMethod cls name m r
  ) =>
  Action cls name m r ->
  MockT m r
mockDefaultlessMethod action =
  withFrozenCallStack $ mockMethodImpl undefined action

-- | An error for an action that matches no expectations at all.  This is only
-- used if severity is Ignore or Warning.
uninterestingError ::
  (HasCallStack, Mockable cls, MonadIO m) =>
  Severity ->
  Action cls name m r ->
  MockT m ()
uninterestingError severity a =
  reportFault severity $ "Uninteresting action: " ++ showAction a

-- | An error for an action that matches no expectations at all.
noMatchError ::
  (HasCallStack, Mockable cls, MonadIO m) =>
  Severity ->
  Action cls name m r ->
  MockT m ()
noMatchError severity a = do
  fullExpectations <- describeExpectations
  reportFault severity $
    "Unexpected action: " ++ showAction a
      ++ "\n\nFull expectations:\n"
      ++ fullExpectations

-- | An error for an action that doesn't match the argument predicates for any
-- of the method's expectations.
partialMatchError ::
  (HasCallStack, Mockable cls, MonadIO m) =>
  Severity ->
  Action cls name m r ->
  [([(Int, String)], String)] ->
  MockT m ()
partialMatchError severity a partials = do
  fullExpectations <- describeExpectations
  reportFault severity $
    "Wrong arguments: "
      ++ showAction a
      ++ "\n\nClosest matches:\n - "
      ++ intercalate "\n - " (map formatPartial $ take 5 partials)
      ++ "\n\nFull expectations:\n"
      ++ fullExpectations
  where
    formatPartial :: ([(Int, String)], String) -> String
    formatPartial (mismatches, matcher)
      | null mismatches = matcher ++ "\n   * Failed whole-method matcher"
      | otherwise =
        matcher ++ "\n   * "
          ++ intercalate
            "\n   * "
            ( map
                ( \(i, mm) ->
                    "Arg #" ++ show i ++ ": " ++ mm
                )
                mismatches
            )

-- | An error for an 'Action' that matches more than one 'Matcher'.  This only
-- triggers an error if ambiguity checks are on.
ambiguityError ::
  (HasCallStack, Mockable cls, MonadIO m) =>
  Severity ->
  Action cls name m r ->
  [String] ->
  MockT m ()
ambiguityError severity a choices = do
  fullExpectations <- describeExpectations
  reportFault severity $
    "Ambiguous action matched multiple expectations: "
      ++ showAction a
      ++ "\n\nMatches:\n - "
      ++ intercalate "\n - " choices
      ++ "\n\nFull expectations:\n"
      ++ fullExpectations
