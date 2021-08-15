-- Do not edit! Automatically created with doctest-extract from src/Test/HMock/Internal/FlowMatcher.hs
{-# LINE 14 "src/Test/HMock/Internal/FlowMatcher.hs" #-}

{-# OPTIONS_GHC -Wno-type-defaults #-}
module DocTests.Test.HMock.Internal.FlowMatcher where

import Test.HMock.Internal.FlowMatcher
import Test.DocTest.Base
import qualified Test.DocTest.Driver as DocTest

{-# LINE 16 "src/Test/HMock/Internal/FlowMatcher.hs" #-}

test :: DocTest.T ()
test = do
 DocTest.printPrefix "Test.HMock.Internal.FlowMatcher:23: "
{-# LINE 23 "src/Test/HMock/Internal/FlowMatcher.hs" #-}
 DocTest.example
{-# LINE 23 "src/Test/HMock/Internal/FlowMatcher.hs" #-}
      (bipartiteMatching (==) [1 .. 5] [6, 5 .. 2])
  [ExpectedLine [LineChunk "([(2,2),(3,3),(4,4),(5,5)],[1],[6])"]]
