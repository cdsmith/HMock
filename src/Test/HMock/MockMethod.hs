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

import Control.Concurrent.STM (readTVar, writeTVar)
import Control.Monad (join)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ask)
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
import Test.HMock.Internal.ExpectSet (ExpectSet, formatExpectSet, liveSteps)
import Test.HMock.Internal.State
  ( MockSetupContext (..),
    MockSetupT (..),
    MockState (..),
    MockT,
    initClassIfNeeded,
    mockSetupSTM,
  )
import Test.HMock.Internal.Step (SingleRule ((:->)), Step (Step))
import Test.HMock.Internal.Util (Located (Loc), withLoc)
import Test.HMock.Mockable (MatchResult (..), Mockable (..), MockableBase (..))

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
    state <- MockSetupT ask
    expectSet <- mockSetupSTM $ readTVar (mockExpectSet state)
    defaults <- mockSetupSTM $ readTVar (mockDefaults state)
    checkAmbig <- mockSetupSTM $ readTVar (mockCheckAmbiguity state)
    let (newExpectSet, response) =
          decideAction expectSet defaults checkAmbig
    mockSetupSTM $ writeTVar (mockExpectSet state) newExpectSet
    return response
  where
    decideAction ::
      ExpectSet (Step m) ->
      [(Bool, Step m)] ->
      Bool ->
      (ExpectSet (Step m), MockT m r)
    decideAction expectSet defaults checkAmbig =
      let (partial, full) = partitionEithers (tryMatch <$> liveSteps expectSet)
          orderedPartial = snd <$> sortBy (compare `on` fst) (catMaybes partial)
       in case (full, surrogate, orderedPartial) of
            (opts@(_ : _ : _), _, _)
              | checkAmbig ->
                error $
                  ambiguityError action ((\(_, s, _) -> s) <$> opts) expectSet
            ((e, _, Just response) : _, _, _) -> (e, response)
            ((e, _, Nothing) : _, s, _)
              | d <- findDefault False defaults -> (e, fromMaybe (return s) d)
            ([], _, _)
              | Just d <- findDefault True defaults -> (expectSet, d)
            ([], _, []) -> error $ noMatchError action expectSet
            ([], _, _) ->
              error $ partialMatchError action orderedPartial expectSet

    tryMatch ::
      (Step m, ExpectSet (Step m)) ->
      Either
        (Maybe (Int, String))
        (ExpectSet (Step m), String, Maybe (MockT m r))
    tryMatch (Step expected, e)
      | Just lrule@(Loc _ (m :-> impl)) <- cast expected =
        case matchAction m action of
          NoMatch n ->
            Left (Just (n, withLoc (showMatcher (Just action) m <$ lrule)))
          Match ->
            Right
              ( e,
                withLoc (lrule $> showMatcher (Just action) m),
                ($ action) <$> impl
              )
      | otherwise = Left Nothing

    findDefault :: Bool -> [(Bool, Step m)] -> Maybe (MockT m r)
    findDefault _ [] = Nothing
    findDefault unexpected ((lax, Step expected) : _)
      | lax || not unexpected,
        Just (Loc _ (m :-> Just r)) <- cast expected,
        Match <- matchAction m action =
        Just (r action)
    findDefault unexpected (_ : steps) = findDefault unexpected steps

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

-- | An error for an action that matches no expectations at all.
noMatchError ::
  Mockable cls => Action cls name m a -> ExpectSet (Step m) -> String
noMatchError a fullExpectations =
  "Unexpected action: " ++ showAction a
    ++ "\n\nFull expectations:\n"
    ++ formatExpectSet fullExpectations

-- | An error for an action that doesn't match the argument predicates for any
-- of the method's expectations.
partialMatchError ::
  Mockable cls =>
  Action cls name m a ->
  [String] ->
  ExpectSet (Step m) ->
  String
partialMatchError a partials fullExpectations =
  "Wrong arguments: "
    ++ showAction a
    ++ "\n\nClosest matches:\n - "
    ++ intercalate "\n - " (take 5 partials)
    ++ "\n\nFull expectations:\n"
    ++ formatExpectSet fullExpectations

-- | An error for an 'Action' that matches more than one 'Matcher'.  This only
-- triggers an error if ambiguity checks are on.
ambiguityError ::
  Mockable cls =>
  Action cls name m a ->
  [String] ->
  ExpectSet (Step m) ->
  String
ambiguityError a choices fullExpectations =
  "Ambiguous action matched multiple expectations: "
    ++ showAction a
    ++ "\n\nMatches:\n - "
    ++ intercalate "\n - " choices
    ++ "\n\nFull expectations:\n"
    ++ formatExpectSet fullExpectations
