-- Do not edit! Automatically created with doctest-extract from src/Test/HMock/Multiplicity.hs
module DocTests.Test.HMock.Multiplicity where

import Test.HMock.Multiplicity
import Test.DocTest.Base
import qualified Test.DocTest.Driver as DocTest


test :: DocTest.T ()
test = do
 DocTest.printPrefix "Test.HMock.Multiplicity:48: "
{-# LINE 48 "src/Test/HMock/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 48 "src/Test/HMock/Multiplicity.hs" #-}
      (meetsMultiplicity 5 4)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Multiplicity:50: "
{-# LINE 50 "src/Test/HMock/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 50 "src/Test/HMock/Multiplicity.hs" #-}
      (meetsMultiplicity 5 5)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Multiplicity:52: "
{-# LINE 52 "src/Test/HMock/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 52 "src/Test/HMock/Multiplicity.hs" #-}
      (between 4 6 - between 1 2)
  [ExpectedLine [LineChunk "2 to 5 times"]]
 DocTest.printPrefix "Test.HMock.Multiplicity:90: "
{-# LINE 90 "src/Test/HMock/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 90 "src/Test/HMock/Multiplicity.hs" #-}
      (meetsMultiplicity once 0)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Multiplicity:92: "
{-# LINE 92 "src/Test/HMock/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 92 "src/Test/HMock/Multiplicity.hs" #-}
      (meetsMultiplicity once 1)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Multiplicity:94: "
{-# LINE 94 "src/Test/HMock/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 94 "src/Test/HMock/Multiplicity.hs" #-}
      (meetsMultiplicity once 2)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Multiplicity:100: "
{-# LINE 100 "src/Test/HMock/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 100 "src/Test/HMock/Multiplicity.hs" #-}
      (meetsMultiplicity anyMultiplicity 0)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Multiplicity:102: "
{-# LINE 102 "src/Test/HMock/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 102 "src/Test/HMock/Multiplicity.hs" #-}
      (meetsMultiplicity anyMultiplicity 1)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Multiplicity:104: "
{-# LINE 104 "src/Test/HMock/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 104 "src/Test/HMock/Multiplicity.hs" #-}
      (meetsMultiplicity anyMultiplicity 10)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Multiplicity:111: "
{-# LINE 111 "src/Test/HMock/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 111 "src/Test/HMock/Multiplicity.hs" #-}
      (meetsMultiplicity (atLeast 2) 1)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Multiplicity:113: "
{-# LINE 113 "src/Test/HMock/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 113 "src/Test/HMock/Multiplicity.hs" #-}
      (meetsMultiplicity (atLeast 2) 2)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Multiplicity:115: "
{-# LINE 115 "src/Test/HMock/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 115 "src/Test/HMock/Multiplicity.hs" #-}
      (meetsMultiplicity (atLeast 2) 3)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Multiplicity:122: "
{-# LINE 122 "src/Test/HMock/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 122 "src/Test/HMock/Multiplicity.hs" #-}
      (meetsMultiplicity (atMost 2) 1)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Multiplicity:124: "
{-# LINE 124 "src/Test/HMock/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 124 "src/Test/HMock/Multiplicity.hs" #-}
      (meetsMultiplicity (atMost 2) 2)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Multiplicity:126: "
{-# LINE 126 "src/Test/HMock/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 126 "src/Test/HMock/Multiplicity.hs" #-}
      (meetsMultiplicity (atMost 2) 3)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Multiplicity:135: "
{-# LINE 135 "src/Test/HMock/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 135 "src/Test/HMock/Multiplicity.hs" #-}
      (meetsMultiplicity (between 2 3) 1)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Multiplicity:137: "
{-# LINE 137 "src/Test/HMock/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 137 "src/Test/HMock/Multiplicity.hs" #-}
      (meetsMultiplicity (between 2 3) 2)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Multiplicity:139: "
{-# LINE 139 "src/Test/HMock/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 139 "src/Test/HMock/Multiplicity.hs" #-}
      (meetsMultiplicity (between 2 3) 3)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Multiplicity:141: "
{-# LINE 141 "src/Test/HMock/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 141 "src/Test/HMock/Multiplicity.hs" #-}
      (meetsMultiplicity (between 2 3) 4)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Multiplicity:148: "
{-# LINE 148 "src/Test/HMock/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 148 "src/Test/HMock/Multiplicity.hs" #-}
      (feasible once)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Multiplicity:150: "
{-# LINE 150 "src/Test/HMock/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 150 "src/Test/HMock/Multiplicity.hs" #-}
      (feasible 0)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Multiplicity:152: "
{-# LINE 152 "src/Test/HMock/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 152 "src/Test/HMock/Multiplicity.hs" #-}
      (feasible (once - 2))
  [ExpectedLine [LineChunk "False"]]
