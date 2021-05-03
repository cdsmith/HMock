-- | This module provides the framework for writing test cases with HMock.
module HMock
  ( MockT,
    runMockT,
    Mockable (Action, Matcher),
    WithResult ((:->)),
    (|->),
    Expected,
    mock,
    expect,
    whenever,
    expectN,
    inSequence,
    Predicate(..),
    eq,
    neq,
    gt,
    geq,
    lt,
    leq,
    __,
    andP,
    orP,
    notP,
    startsWith,
    endsWith,
    hasSubstr,
    suchThat,
    Cardinality,
    once,
    times,
    atLeast,
    atMost,
    anyCardinality,
  )
where

import HMock.Internal.Core
import HMock.Internal.Predicates
import HMock.Internal.Cardinality