{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

-- | The internal core language of expectations in HMock.
module Test.HMock.Internal.ExpectSet where

import Test.HMock.Multiplicity
  ( Multiplicity,
    between,
    feasible, meetsMultiplicity
  )

-- | A set of expected steps and their responses.  This is the "core" language
-- of expectations for HMock.  It's based roughly on Svenningsson, Svensson,
-- Smallbone, Arts, Norell, and Hughes' Expressive Semantics of Mocking.
-- However, there are a few small adjustments.  We have two repetition operators
-- which respectively represent general repetition with interleaving, and
-- consecutive repetition.  We also attach arbitrary multiplicities to
-- repetition.
data ExpectSet step where
  ExpectStep :: step -> ExpectSet step
  ExpectNothing :: ExpectSet step
  ExpectSequence :: ExpectSet step -> ExpectSet step -> ExpectSet step
  ExpectInterleave :: ExpectSet step -> ExpectSet step -> ExpectSet step
  ExpectEither :: ExpectSet step -> ExpectSet step -> ExpectSet step
  ExpectMulti :: Multiplicity -> ExpectSet step -> ExpectSet step
  ExpectConsecutive :: Multiplicity -> ExpectSet step -> ExpectSet step
  deriving (Show, Eq)

-- | Checks whether an ExpectSet is in an "accepting" state.  In other words, is
-- it okay for the test to end here?  If False, then there are still
-- expectations that must be satisfied before the test can succeed.
satisfied :: ExpectSet step -> Bool
satisfied (ExpectStep _) = False
satisfied ExpectNothing = True
satisfied (ExpectSequence e f) = satisfied e && satisfied f
satisfied (ExpectInterleave e f) = satisfied e && satisfied f
satisfied (ExpectEither e f) = satisfied e || satisfied f
satisfied (ExpectMulti mult e) =
  feasible mult && (meetsMultiplicity mult 0 || satisfied e)
satisfied (ExpectConsecutive mult e) =
  feasible mult && (meetsMultiplicity mult 0 || satisfied e)

-- | Computes the live steps of the ExpectSet.  In other words: which individual
-- steps can be matched right now, and what are the remaining expectations in
-- each case?
liveSteps :: ExpectSet step -> [(step, ExpectSet step)]
liveSteps (ExpectStep step) = [(step, ExpectNothing)]
liveSteps ExpectNothing = []
liveSteps (ExpectSequence e f) =
  (fmap (`ExpectSequence` f) <$> liveSteps e)
    ++ if satisfied e then liveSteps f else []
liveSteps (ExpectInterleave e f) =
  (fmap (`ExpectInterleave` f) <$> liveSteps e)
    ++ (fmap (ExpectInterleave e) <$> liveSteps f)
liveSteps (ExpectEither e f) = liveSteps e ++ liveSteps f
liveSteps (ExpectMulti mult e)
  | feasible (mult - 1) =
    [ (step, ExpectInterleave f (ExpectMulti (mult - 1) e))
      | (step, f) <- liveSteps e
    ]
  | otherwise = []
liveSteps (ExpectConsecutive mult e)
  | feasible (mult - 1) =
    [ (step, ExpectSequence f (ExpectConsecutive (mult - 1) e))
      | (step, f) <- liveSteps e
    ]
  | otherwise = []

-- | Performs a complete simplification of the ExpectSet.  This could be slow,
-- but we intend to do it only for error messages, so it need not be very fast.
simplify :: ExpectSet step -> ExpectSet step
simplify (ExpectSequence e f)
  | ExpectNothing <- e' = f'
  | ExpectNothing <- f' = e'
  | ExpectSequence e1 e2 <- e' =
    simplify (ExpectSequence e1 (ExpectSequence e2 f'))
  | otherwise = ExpectSequence e' f'
  where
    e' = simplify e
    f' = simplify f
simplify (ExpectInterleave e f)
  | ExpectNothing <- e' = f'
  | ExpectNothing <- f' = e'
  | ExpectInterleave e1 e2 <- e' =
    simplify (ExpectInterleave e1 (ExpectInterleave e2 f'))
  | otherwise = ExpectInterleave e' f'
  where
    e' = simplify e
    f' = simplify f
simplify (ExpectEither e f)
  | ExpectNothing <- e', ExpectNothing <- f' = ExpectNothing
  | ExpectNothing <- e' = simplify (ExpectEither f' ExpectNothing)
  | ExpectEither e1 e2 <- e' =
    simplify (ExpectEither e1 (ExpectEither e2 f'))
  | ExpectNothing <- f', satisfied e' = e'
  | ExpectNothing <- f' = simplify (ExpectMulti (between 0 1) e')
  | otherwise = ExpectEither e' f'
  where
    e' = simplify e
    f' = simplify f
simplify (ExpectMulti m e)
  | not (feasible m) = ExpectMulti m ExpectNothing 
  | ExpectNothing <- e' = ExpectNothing
  | m == 0 = ExpectNothing
  | m == 1 = e'
  | otherwise = ExpectMulti m e'
  where
    e' = simplify e
simplify (ExpectConsecutive m e)
  | not (feasible m) = ExpectConsecutive m ExpectNothing 
  | ExpectNothing <- e' = ExpectNothing
  | m == 0 = ExpectNothing
  | m == 1 = e'
  | otherwise = ExpectConsecutive m e'
  where
    e' = simplify e
simplify other = other

-- | Get a list of all steps mentioned by an 'ExpectSet'.  This is used to
-- determine which classes need to be initialized before adding an expectation.
getSteps :: ExpectSet step -> [step]
getSteps ExpectNothing = []
getSteps (ExpectStep step) = [step]
getSteps (ExpectInterleave e f) = getSteps e ++ getSteps f
getSteps (ExpectSequence e f) = getSteps e ++ getSteps f
getSteps (ExpectEither e f) = getSteps e ++ getSteps f
getSteps (ExpectMulti _ e) = getSteps e
getSteps (ExpectConsecutive _ e) = getSteps e

-- | A higher-level intermediate form of an ExpectSet suitable for communication
-- with the user.  Chains of binary operators are collected into sequences to
-- be displayed in lists rather than arbitrary nesting.
data CollectedSet step where
  CollectedStep :: step -> CollectedSet step
  CollectedNothing :: CollectedSet step
  CollectedSequence :: [CollectedSet step] -> CollectedSet step
  CollectedInterleave :: [CollectedSet step] -> CollectedSet step
  CollectedChoice :: [CollectedSet step] -> CollectedSet step
  CollectedMulti :: Multiplicity -> CollectedSet step -> CollectedSet step
  CollectedConsecutive :: Multiplicity -> CollectedSet step -> CollectedSet step

-- | Collects an ExpectSet into the intermediate form for display.  It's assumed
-- that the expression was simplified before this operation.
collect :: ExpectSet step -> CollectedSet step
collect (ExpectStep s) = CollectedStep s
collect ExpectNothing = CollectedNothing
collect (ExpectSequence e f) = CollectedSequence (collect e : fs)
  where
    fs = case collect f of
      CollectedSequence f' -> f'
      f' -> [f']
collect (ExpectInterleave e f) = CollectedInterleave (collect e : fs)
  where
    fs = case collect f of
      CollectedInterleave f' -> f'
      f' -> [f']
collect (ExpectEither e f) = CollectedChoice (collect e : fs)
  where
    fs = case collect f of
      CollectedChoice f' -> f'
      f' -> [f']
collect (ExpectMulti m e) = CollectedMulti m (collect e)
collect (ExpectConsecutive m e) = CollectedConsecutive m (collect e)

-- | Converts a set of expectations into a string that summarizes them, with
-- the given prefix (used to indent).
formatExpectSet :: (Show step) => ExpectSet step -> String
formatExpectSet = go "" . collect . simplify
  where
    go prefix CollectedNothing = prefix ++ "* nothing"
    go prefix (CollectedStep step) = prefix ++ "* " ++ show step
    go prefix (CollectedSequence cs) =
      prefix ++ "* in sequence:\n" ++ unlines (map (go ("    " ++ prefix)) cs)
    go prefix (CollectedInterleave cs) =
      prefix ++ "* in any order:\n" ++ unlines (map (go ("    " ++ prefix)) cs)
    go prefix (CollectedChoice cs) =
      prefix ++ "* any of:\n" ++ unlines (map (go ("    " ++ prefix)) cs)
    go prefix (CollectedMulti m e) =
      prefix ++ "* " ++ show m ++ ":\n" ++ go ("    " ++ prefix) e
    go prefix (CollectedConsecutive m e) =
      prefix ++ "* " ++ show m ++ " consecutively:\n" ++ go ("    " ++ prefix) e

-- | Reduces a set of expectations to the minimum steps that would be required
-- to satisfy the entire set.  This weeds out unnecessary information before
-- reporting that there were unmet expectations at the end of the test.
excess :: ExpectSet step -> ExpectSet step
excess = simplify . go
  where
    go (ExpectSequence e f) = ExpectSequence (go e) (go f)
    go (ExpectInterleave e f) = ExpectInterleave (go e) (go f)
    go (ExpectEither e f)
      | satisfied e || satisfied f = ExpectNothing
      | otherwise = ExpectEither (go e) (go f)
    go (ExpectMulti m e)
      | meetsMultiplicity m 0 = ExpectNothing
      | otherwise = ExpectMulti m (go e)
    go (ExpectConsecutive m e)
      | meetsMultiplicity m 0 = ExpectNothing
      | otherwise = ExpectConsecutive m (go e)
    go other = other
