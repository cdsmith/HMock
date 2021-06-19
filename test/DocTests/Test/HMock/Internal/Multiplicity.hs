-- Do not edit! Automatically created with doctest-extract from src/Test/HMock/Internal/Multiplicity.hs
module DocTests.Test.HMock.Internal.Multiplicity where

import Test.HMock.Internal.Multiplicity
import Test.DocTest.Base
import qualified Test.DocTest.Driver as DocTest


test :: DocTest.T ()
test = do
 DocTest.printPrefix "Test.HMock.Internal.Multiplicity:26: "
{-# LINE 26 "src/Test/HMock/Internal/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 26 "src/Test/HMock/Internal/Multiplicity.hs" #-}
      (meetsMultiplicity 5 4)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Multiplicity:28: "
{-# LINE 28 "src/Test/HMock/Internal/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 28 "src/Test/HMock/Internal/Multiplicity.hs" #-}
      (meetsMultiplicity 5 5)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Multiplicity:30: "
{-# LINE 30 "src/Test/HMock/Internal/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 30 "src/Test/HMock/Internal/Multiplicity.hs" #-}
      (between 4 6 - between 1 2)
  [ExpectedLine [LineChunk "2 to 5 times"]]
 DocTest.printPrefix "Test.HMock.Internal.Multiplicity:61: "
{-# LINE 61 "src/Test/HMock/Internal/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 61 "src/Test/HMock/Internal/Multiplicity.hs" #-}
      (meetsMultiplicity once 0)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Multiplicity:63: "
{-# LINE 63 "src/Test/HMock/Internal/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 63 "src/Test/HMock/Internal/Multiplicity.hs" #-}
      (meetsMultiplicity once 1)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Multiplicity:65: "
{-# LINE 65 "src/Test/HMock/Internal/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 65 "src/Test/HMock/Internal/Multiplicity.hs" #-}
      (meetsMultiplicity once 2)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Multiplicity:71: "
{-# LINE 71 "src/Test/HMock/Internal/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 71 "src/Test/HMock/Internal/Multiplicity.hs" #-}
      (meetsMultiplicity anyMultiplicity 0)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Multiplicity:73: "
{-# LINE 73 "src/Test/HMock/Internal/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 73 "src/Test/HMock/Internal/Multiplicity.hs" #-}
      (meetsMultiplicity anyMultiplicity 1)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Multiplicity:75: "
{-# LINE 75 "src/Test/HMock/Internal/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 75 "src/Test/HMock/Internal/Multiplicity.hs" #-}
      (meetsMultiplicity anyMultiplicity 10)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Multiplicity:82: "
{-# LINE 82 "src/Test/HMock/Internal/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 82 "src/Test/HMock/Internal/Multiplicity.hs" #-}
      (meetsMultiplicity (atLeast 2) 1)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Multiplicity:84: "
{-# LINE 84 "src/Test/HMock/Internal/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 84 "src/Test/HMock/Internal/Multiplicity.hs" #-}
      (meetsMultiplicity (atLeast 2) 2)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Multiplicity:86: "
{-# LINE 86 "src/Test/HMock/Internal/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 86 "src/Test/HMock/Internal/Multiplicity.hs" #-}
      (meetsMultiplicity (atLeast 2) 3)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Multiplicity:93: "
{-# LINE 93 "src/Test/HMock/Internal/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 93 "src/Test/HMock/Internal/Multiplicity.hs" #-}
      (meetsMultiplicity (atMost 2) 1)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Multiplicity:95: "
{-# LINE 95 "src/Test/HMock/Internal/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 95 "src/Test/HMock/Internal/Multiplicity.hs" #-}
      (meetsMultiplicity (atMost 2) 2)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Multiplicity:97: "
{-# LINE 97 "src/Test/HMock/Internal/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 97 "src/Test/HMock/Internal/Multiplicity.hs" #-}
      (meetsMultiplicity (atMost 2) 3)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Multiplicity:106: "
{-# LINE 106 "src/Test/HMock/Internal/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 106 "src/Test/HMock/Internal/Multiplicity.hs" #-}
      (meetsMultiplicity (between 2 3) 1)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Multiplicity:108: "
{-# LINE 108 "src/Test/HMock/Internal/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 108 "src/Test/HMock/Internal/Multiplicity.hs" #-}
      (meetsMultiplicity (between 2 3) 2)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Multiplicity:110: "
{-# LINE 110 "src/Test/HMock/Internal/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 110 "src/Test/HMock/Internal/Multiplicity.hs" #-}
      (meetsMultiplicity (between 2 3) 3)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Multiplicity:112: "
{-# LINE 112 "src/Test/HMock/Internal/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 112 "src/Test/HMock/Internal/Multiplicity.hs" #-}
      (meetsMultiplicity (between 2 3) 4)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Multiplicity:119: "
{-# LINE 119 "src/Test/HMock/Internal/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 119 "src/Test/HMock/Internal/Multiplicity.hs" #-}
      (exhaustable anyMultiplicity)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Multiplicity:121: "
{-# LINE 121 "src/Test/HMock/Internal/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 121 "src/Test/HMock/Internal/Multiplicity.hs" #-}
      (exhaustable (atLeast 2))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Multiplicity:123: "
{-# LINE 123 "src/Test/HMock/Internal/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 123 "src/Test/HMock/Internal/Multiplicity.hs" #-}
      (exhaustable (atMost 3))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Multiplicity:125: "
{-# LINE 125 "src/Test/HMock/Internal/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 125 "src/Test/HMock/Internal/Multiplicity.hs" #-}
      (exhaustable (between 0 2))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Multiplicity:132: "
{-# LINE 132 "src/Test/HMock/Internal/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 132 "src/Test/HMock/Internal/Multiplicity.hs" #-}
      (satisfiable once)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Multiplicity:134: "
{-# LINE 134 "src/Test/HMock/Internal/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 134 "src/Test/HMock/Internal/Multiplicity.hs" #-}
      (satisfiable 0)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Multiplicity:136: "
{-# LINE 136 "src/Test/HMock/Internal/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 136 "src/Test/HMock/Internal/Multiplicity.hs" #-}
      (satisfiable (once - 2))
  [ExpectedLine [LineChunk "False"]]
