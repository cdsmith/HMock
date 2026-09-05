{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | The definitions behind "Test.HMock.Bluefin": the source and mock handles,
-- what an expectation can target, and the plumbing the public module shares.
module Test.HMock.Bluefin.Internal where

import Bluefin.Compound
  ( Handle,
    OneWayCoercible (..),
    OneWayCoercibleHandle (..),
    mapHandle,
    oneWayCoercibleTrustMe,
    useImplIn,
  )
import Bluefin.Eff (Eff, Effects, type (:&), type (<:))
import Bluefin.IO (IOE, withEffToIO_)
import Data.Kind (Constraint, Type)
import Test.HMock.Internal.State (MockState)
import Test.HMock.Internal.Util (InstanceId)

-- | Creates mocks, as 'Bluefin.StateSource.StateSource' creates states.
--
-- A source owns one layer of expectations, shared by every t'Mock' made from
-- it, so a plan can span all of them.  'Test.HMock.Bluefin.runMock' provides the outermost layer
-- and 'Test.HMock.Bluefin.nestMockSource' a nested one.  Test-wide settings and
-- 'Test.HMock.Bluefin.verifyExpectations' take the source too.
--
-- [@root@] The 'Test.HMock.Bluefin.runMock' this source belongs to.  Every source and mock in a
--   test shares it, so a mock can't be adopted into another test's source.
--
-- [@resp@] The scope responses run in: the body of the 'Test.HMock.Bluefin.runMock' or
--   'Test.HMock.Bluefin.nestMockSource' that made the source.  A response attached with 'Test.HMock.Bluefin.|=>'
--   may use any handle in scope there.
--
-- [@e@] The handle's own effect tag, as for any Bluefin handle.
type role MockSource nominal nominal nominal

data MockSource (root :: Effects) (resp :: Effects) (e :: Effects)
  = MockSource (MockState IO) (IOE e) (Unlifter resp)
  deriving (Handle) via OneWayCoercibleHandle (MockSource root resp)

instance
  (e <: es) =>
  OneWayCoercible (MockSource root resp e) (MockSource root resp es)
  where
  oneWayCoercibleImpl =
    oneWayCoercibleTrustMe
      (\(MockSource st io unlift) -> MockSource st (mapHandle io) unlift)

-- | One mocked instance of the capability @h@.
--
-- This is the handle you attach expectations to; the 'Test.HMock.Bluefin.mocked' accessor turns
-- it into the plain capability record that the code under test expects.  Two
-- @Mock FileSystem@s from separate 'Test.HMock.Bluefin.newMock' calls are distinct instances,
-- and a call through one will not match an expectation set on the other.
--
-- @root@ and @resp@ come from the source that made it; see t'MockSource'.
-- @e@ is the handle's own effect tag.
type role Mock nominal nominal nominal nominal

data Mock (root :: Effects) (resp :: Effects) (h :: Effects -> Type) (e :: Effects)
  = Mock (MockState IO) InstanceId (IOE e) (Unlifter resp)
  deriving (Handle) via OneWayCoercibleHandle (Mock root resp h)

instance
  (e <: es) =>
  OneWayCoercible (Mock root resp h e) (Mock root resp h es)
  where
  oneWayCoercibleImpl =
    oneWayCoercibleTrustMe
      (\(Mock st cid io unlift) -> Mock st cid (mapHandle io) unlift)

-- | The layer a mock's rules and calls go to.
mockState :: Mock root resp h e -> MockState IO
mockState (Mock st _ _ _) = st

-- | The instance a mock's rules and calls are tagged with.
mockId :: Mock root resp h e -> Maybe InstanceId
mockId (Mock _ cid _ _) = Just cid

-- | A mock's own 'IOE'.
mockIO :: Mock root resp h e -> IOE e
mockIO (Mock _ _ io _) = io

-- | Runs responses in a mock's response scope.
mockUnlift :: Mock root resp h e -> Unlifter resp
mockUnlift (Mock _ _ _ unlift) = unlift

-- | Runs actions in a source's response scope.  Captured when 'Test.HMock.Bluefin.runMock' or
-- 'Test.HMock.Bluefin.nestMockSource' opens that scope, and carried by every mock made from the
-- source, so a response can be installed from anywhere the mock is.
newtype Unlifter resp = Unlifter (forall r. Eff resp r -> IO r)

-- | What an expectation can be attached to.
--
-- A t'Mock' targets one instance: only calls through that instance match.  A
-- t'MockSource' targets every instance of a capability made from it, present
-- or future; the capability is whichever one the rule mentions.
--
-- @
-- 'Test.HMock.Bluefin.expect' fs '$' ReadFile "a.txt" 'Test.HMock.Bluefin.|->' "x"        -- this instance only
-- 'Test.HMock.Bluefin.expect' source '$' ReadFile "a.txt" 'Test.HMock.Bluefin.|->' "x"    -- any FileSystem from source
-- @
class Target (t :: Effects -> Type) (resp :: Effects) | t -> resp where
  -- | Whether the target can carry rules for the capability @h@: a mock only
  -- for its own capability, a source for any.
  type Accepts t (h :: Effects -> Type) :: Constraint

  -- | The layer the target's rules go into.
  targetState :: t c -> MockState IO

  -- | The instance the target's rules are tagged with, if any.
  targetId :: t c -> Maybe InstanceId

  -- | The target's own 'IOE'.
  targetIO :: t c -> IOE c

  -- | Runs responses in the target's response scope.
  targetUnlift :: t c -> Unlifter resp

instance Target (Mock root resp h) resp where
  type Accepts (Mock root resp h) h' = h' ~ h
  targetState = mockState
  targetId = mockId
  targetIO = mockIO
  targetUnlift = mockUnlift

instance Target (MockSource root resp) resp where
  type Accepts (MockSource root resp) h = ()
  targetState (MockSource st _ _) = st
  targetId _ = Nothing
  targetIO (MockSource _ io _) = io
  targetUnlift (MockSource _ _ unlift) = unlift


-- | Opens a fresh scope over @es@ with an 'IOE' and an t'Unlifter' for it, and
-- runs the body there.
withScope ::
  forall es e1 a.
  (e1 <: es) =>
  IOE e1 ->
  (IOE es -> Unlifter (es :& es) -> Eff (es :& es) a) ->
  Eff es a
withScope io body =
  useImplIn
    ( \() ->
        withEffToIO_ (mapHandle io :: IOE (es :& es)) $ \unlift ->
          unlift (body (mapHandle io) (Unlifter unlift))
    )
    ()
