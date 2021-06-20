-- Do not edit! Automatically created with doctest-extract from src/Test/HMock/Internal/Multiplicity.hs
module DocTests.Test.HMock.Internal.Multiplicity where

import Test.HMock.Internal.Multiplicity
import Test.DocTest.Base
import qualified Test.DocTest.Driver as DocTest


test :: DocTest.T ()
test = do
 DocTest.printPrefix "Test.HMock.Internal.Multiplicity:33: "
{-# LINE 33 "src/Test/HMock/Internal/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 33 "src/Test/HMock/Internal/Multiplicity.hs" #-}
      (meetsMultiplicity 5 4)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Multiplicity:35: "
{-# LINE 35 "src/Test/HMock/Internal/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 35 "src/Test/HMock/Internal/Multiplicity.hs" #-}
      (meetsMultiplicity 5 5)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Multiplicity:37: "
{-# LINE 37 "src/Test/HMock/Internal/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 37 "src/Test/HMock/Internal/Multiplicity.hs" #-}
      (between 4 6 - between 1 2)
  [ExpectedLine [LineChunk "2 to 5 times"]]
 DocTest.printPrefix "Test.HMock.Internal.Multiplicity:75: "
{-# LINE 75 "src/Test/HMock/Internal/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 75 "src/Test/HMock/Internal/Multiplicity.hs" #-}
      (meetsMultiplicity once 0)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Multiplicity:77: "
{-# LINE 77 "src/Test/HMock/Internal/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 77 "src/Test/HMock/Internal/Multiplicity.hs" #-}
      (meetsMultiplicity once 1)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Multiplicity:79: "
{-# LINE 79 "src/Test/HMock/Internal/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 79 "src/Test/HMock/Internal/Multiplicity.hs" #-}
      (meetsMultiplicity once 2)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Multiplicity:85: "
{-# LINE 85 "src/Test/HMock/Internal/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 85 "src/Test/HMock/Internal/Multiplicity.hs" #-}
      (meetsMultiplicity anyMultiplicity 0)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Multiplicity:87: "
{-# LINE 87 "src/Test/HMock/Internal/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 87 "src/Test/HMock/Internal/Multiplicity.hs" #-}
      (meetsMultiplicity anyMultiplicity 1)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Multiplicity:89: "
{-# LINE 89 "src/Test/HMock/Internal/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 89 "src/Test/HMock/Internal/Multiplicity.hs" #-}
      (meetsMultiplicity anyMultiplicity 10)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Multiplicity:96: "
{-# LINE 96 "src/Test/HMock/Internal/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 96 "src/Test/HMock/Internal/Multiplicity.hs" #-}
      (meetsMultiplicity (atLeast 2) 1)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Multiplicity:98: "
{-# LINE 98 "src/Test/HMock/Internal/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 98 "src/Test/HMock/Internal/Multiplicity.hs" #-}
      (meetsMultiplicity (atLeast 2) 2)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Multiplicity:100: "
{-# LINE 100 "src/Test/HMock/Internal/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 100 "src/Test/HMock/Internal/Multiplicity.hs" #-}
      (meetsMultiplicity (atLeast 2) 3)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Multiplicity:107: "
{-# LINE 107 "src/Test/HMock/Internal/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 107 "src/Test/HMock/Internal/Multiplicity.hs" #-}
      (meetsMultiplicity (atMost 2) 1)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Multiplicity:109: "
{-# LINE 109 "src/Test/HMock/Internal/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 109 "src/Test/HMock/Internal/Multiplicity.hs" #-}
      (meetsMultiplicity (atMost 2) 2)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Multiplicity:111: "
{-# LINE 111 "src/Test/HMock/Internal/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 111 "src/Test/HMock/Internal/Multiplicity.hs" #-}
      (meetsMultiplicity (atMost 2) 3)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Multiplicity:120: "
{-# LINE 120 "src/Test/HMock/Internal/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 120 "src/Test/HMock/Internal/Multiplicity.hs" #-}
      (meetsMultiplicity (between 2 3) 1)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Multiplicity:122: "
{-# LINE 122 "src/Test/HMock/Internal/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 122 "src/Test/HMock/Internal/Multiplicity.hs" #-}
      (meetsMultiplicity (between 2 3) 2)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Multiplicity:124: "
{-# LINE 124 "src/Test/HMock/Internal/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 124 "src/Test/HMock/Internal/Multiplicity.hs" #-}
      (meetsMultiplicity (between 2 3) 3)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Multiplicity:126: "
{-# LINE 126 "src/Test/HMock/Internal/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 126 "src/Test/HMock/Internal/Multiplicity.hs" #-}
      (meetsMultiplicity (between 2 3) 4)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Multiplicity:133: "
{-# LINE 133 "src/Test/HMock/Internal/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 133 "src/Test/HMock/Internal/Multiplicity.hs" #-}
      (exhaustable anyMultiplicity)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Multiplicity:135: "
{-# LINE 135 "src/Test/HMock/Internal/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 135 "src/Test/HMock/Internal/Multiplicity.hs" #-}
      (exhaustable (atLeast 2))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Multiplicity:137: "
{-# LINE 137 "src/Test/HMock/Internal/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 137 "src/Test/HMock/Internal/Multiplicity.hs" #-}
      (exhaustable (atMost 3))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Multiplicity:139: "
{-# LINE 139 "src/Test/HMock/Internal/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 139 "src/Test/HMock/Internal/Multiplicity.hs" #-}
      (exhaustable (between 0 2))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Multiplicity:146: "
{-# LINE 146 "src/Test/HMock/Internal/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 146 "src/Test/HMock/Internal/Multiplicity.hs" #-}
      (feasible once)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Multiplicity:148: "
{-# LINE 148 "src/Test/HMock/Internal/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 148 "src/Test/HMock/Internal/Multiplicity.hs" #-}
      (feasible 0)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Multiplicity:150: "
{-# LINE 150 "src/Test/HMock/Internal/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 150 "src/Test/HMock/Internal/Multiplicity.hs" #-}
      (feasible (once - 2))
  [ExpectedLine [LineChunk "False"]]
