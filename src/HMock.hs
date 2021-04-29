-- | This module provides the framework for writing test cases with HMock.
module HMock
  ( MockT,
    runMockT,
    Mockable (Action, Matcher),
    ExactMockable (exactly),
    WithResult ((:=>)),
    (|=>),
    (|->),
    Expected,
    mock,
    expect,
    whenever,
    expectN,
    inSequence,
    Predicate(..),
    eq_,
    neq_,
    gt_,
    geq_,
    lt_,
    leq_,
    any_,
    none_,
    and_,
    or_,
    not_,
    startsWith_,
    endsWith_,
    hasSubstr_,
    suchThat_,
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