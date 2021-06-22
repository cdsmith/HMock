-- Do not edit! Automatically created with doctest-extract from src/Test/HMock/Multiplicity.hs
module DocTests.Test.HMock.Multiplicity where

import Test.HMock.Multiplicity
import Test.DocTest.Base
import qualified Test.DocTest.Driver as DocTest


test :: DocTest.T ()
test = do
 DocTest.printPrefix "Test.HMock.Multiplicity:47: "
{-# LINE 47 "src/Test/HMock/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 47 "src/Test/HMock/Multiplicity.hs" #-}
      (meetsMultiplicity 5 4)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Multiplicity:49: "
{-# LINE 49 "src/Test/HMock/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 49 "src/Test/HMock/Multiplicity.hs" #-}
      (meetsMultiplicity 5 5)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Multiplicity:51: "
{-# LINE 51 "src/Test/HMock/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 51 "src/Test/HMock/Multiplicity.hs" #-}
      (between 4 6 - between 1 2)
  [ExpectedLine [LineChunk "2 to 5 times"]]
 DocTest.printPrefix "Test.HMock.Multiplicity:89: "
{-# LINE 89 "src/Test/HMock/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 89 "src/Test/HMock/Multiplicity.hs" #-}
      (meetsMultiplicity once 0)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Multiplicity:91: "
{-# LINE 91 "src/Test/HMock/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 91 "src/Test/HMock/Multiplicity.hs" #-}
      (meetsMultiplicity once 1)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Multiplicity:93: "
{-# LINE 93 "src/Test/HMock/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 93 "src/Test/HMock/Multiplicity.hs" #-}
      (meetsMultiplicity once 2)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Multiplicity:99: "
{-# LINE 99 "src/Test/HMock/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 99 "src/Test/HMock/Multiplicity.hs" #-}
      (meetsMultiplicity anyMultiplicity 0)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Multiplicity:101: "
{-# LINE 101 "src/Test/HMock/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 101 "src/Test/HMock/Multiplicity.hs" #-}
      (meetsMultiplicity anyMultiplicity 1)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Multiplicity:103: "
{-# LINE 103 "src/Test/HMock/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 103 "src/Test/HMock/Multiplicity.hs" #-}
      (meetsMultiplicity anyMultiplicity 10)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Multiplicity:110: "
{-# LINE 110 "src/Test/HMock/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 110 "src/Test/HMock/Multiplicity.hs" #-}
      (meetsMultiplicity (atLeast 2) 1)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Multiplicity:112: "
{-# LINE 112 "src/Test/HMock/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 112 "src/Test/HMock/Multiplicity.hs" #-}
      (meetsMultiplicity (atLeast 2) 2)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Multiplicity:114: "
{-# LINE 114 "src/Test/HMock/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 114 "src/Test/HMock/Multiplicity.hs" #-}
      (meetsMultiplicity (atLeast 2) 3)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Multiplicity:121: "
{-# LINE 121 "src/Test/HMock/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 121 "src/Test/HMock/Multiplicity.hs" #-}
      (meetsMultiplicity (atMost 2) 1)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Multiplicity:123: "
{-# LINE 123 "src/Test/HMock/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 123 "src/Test/HMock/Multiplicity.hs" #-}
      (meetsMultiplicity (atMost 2) 2)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Multiplicity:125: "
{-# LINE 125 "src/Test/HMock/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 125 "src/Test/HMock/Multiplicity.hs" #-}
      (meetsMultiplicity (atMost 2) 3)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Multiplicity:134: "
{-# LINE 134 "src/Test/HMock/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 134 "src/Test/HMock/Multiplicity.hs" #-}
      (meetsMultiplicity (between 2 3) 1)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Multiplicity:136: "
{-# LINE 136 "src/Test/HMock/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 136 "src/Test/HMock/Multiplicity.hs" #-}
      (meetsMultiplicity (between 2 3) 2)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Multiplicity:138: "
{-# LINE 138 "src/Test/HMock/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 138 "src/Test/HMock/Multiplicity.hs" #-}
      (meetsMultiplicity (between 2 3) 3)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Multiplicity:140: "
{-# LINE 140 "src/Test/HMock/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 140 "src/Test/HMock/Multiplicity.hs" #-}
      (meetsMultiplicity (between 2 3) 4)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Multiplicity:147: "
{-# LINE 147 "src/Test/HMock/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 147 "src/Test/HMock/Multiplicity.hs" #-}
      (feasible once)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Multiplicity:149: "
{-# LINE 149 "src/Test/HMock/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 149 "src/Test/HMock/Multiplicity.hs" #-}
      (feasible 0)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Multiplicity:151: "
{-# LINE 151 "src/Test/HMock/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 151 "src/Test/HMock/Multiplicity.hs" #-}
      (feasible (once - 2))
  [ExpectedLine [LineChunk "False"]]
