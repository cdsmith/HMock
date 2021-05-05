-- | This module provides the framework for writing test cases with HMock.
module HMock
  ( MockT,
    runMockT,
    Mockable (Action, Matcher),
    Rule ((:->)),
    (|->),
    Expected,
    mock,
    expect,
    expectN,
    expectAny,
    whenever,
    inSequence,
    inAnyOrder,
    Predicate (..),
    eq,
    neq,
    gt,
    geq,
    lt,
    leq,
    anything,
    andP,
    orP,
    notP,
    startsWith,
    endsWith,
    hasSubstr,
    suchThat,
    typed,
    Cardinality,
    once,
    anyCardinality,
    exactly,
    atLeast,
    atMost,
    interval,
  )
where

import HMock.Internal.Cardinality
import HMock.Internal.Core
import HMock.Internal.Predicates
