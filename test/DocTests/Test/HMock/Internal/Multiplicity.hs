-- Do not edit! Automatically created with doctest-extract from src/Test/HMock/Internal/Multiplicity.hs
module DocTests.Test.HMock.Internal.Multiplicity where

import Test.HMock.Internal.Multiplicity
import Test.DocTest.Base
import qualified Test.DocTest.Driver as DocTest


test :: DocTest.T ()
test = do
 DocTest.printPrefix "Test.HMock.Internal.Multiplicity:42: "
{-# LINE 42 "src/Test/HMock/Internal/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 42 "src/Test/HMock/Internal/Multiplicity.hs" #-}
      (meetsMultiplicity (exactly 5) 4)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Multiplicity:44: "
{-# LINE 44 "src/Test/HMock/Internal/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 44 "src/Test/HMock/Internal/Multiplicity.hs" #-}
      (meetsMultiplicity (exactly 5) 5)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Multiplicity:46: "
{-# LINE 46 "src/Test/HMock/Internal/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 46 "src/Test/HMock/Internal/Multiplicity.hs" #-}
      (meetsMultiplicity (exactly 5) 6)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Multiplicity:53: "
{-# LINE 53 "src/Test/HMock/Internal/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 53 "src/Test/HMock/Internal/Multiplicity.hs" #-}
      (meetsMultiplicity once 0)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Multiplicity:55: "
{-# LINE 55 "src/Test/HMock/Internal/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 55 "src/Test/HMock/Internal/Multiplicity.hs" #-}
      (meetsMultiplicity once 1)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Multiplicity:57: "
{-# LINE 57 "src/Test/HMock/Internal/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 57 "src/Test/HMock/Internal/Multiplicity.hs" #-}
      (meetsMultiplicity once 2)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Multiplicity:63: "
{-# LINE 63 "src/Test/HMock/Internal/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 63 "src/Test/HMock/Internal/Multiplicity.hs" #-}
      (meetsMultiplicity anyMultiplicity 0)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Multiplicity:65: "
{-# LINE 65 "src/Test/HMock/Internal/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 65 "src/Test/HMock/Internal/Multiplicity.hs" #-}
      (meetsMultiplicity anyMultiplicity 1)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Multiplicity:67: "
{-# LINE 67 "src/Test/HMock/Internal/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 67 "src/Test/HMock/Internal/Multiplicity.hs" #-}
      (meetsMultiplicity anyMultiplicity 10)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Multiplicity:74: "
{-# LINE 74 "src/Test/HMock/Internal/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 74 "src/Test/HMock/Internal/Multiplicity.hs" #-}
      (meetsMultiplicity (atLeast 2) 1)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Multiplicity:76: "
{-# LINE 76 "src/Test/HMock/Internal/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 76 "src/Test/HMock/Internal/Multiplicity.hs" #-}
      (meetsMultiplicity (atLeast 2) 2)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Multiplicity:78: "
{-# LINE 78 "src/Test/HMock/Internal/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 78 "src/Test/HMock/Internal/Multiplicity.hs" #-}
      (meetsMultiplicity (atLeast 2) 3)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Multiplicity:85: "
{-# LINE 85 "src/Test/HMock/Internal/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 85 "src/Test/HMock/Internal/Multiplicity.hs" #-}
      (meetsMultiplicity (atMost 2) 1)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Multiplicity:87: "
{-# LINE 87 "src/Test/HMock/Internal/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 87 "src/Test/HMock/Internal/Multiplicity.hs" #-}
      (meetsMultiplicity (atMost 2) 2)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Multiplicity:89: "
{-# LINE 89 "src/Test/HMock/Internal/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 89 "src/Test/HMock/Internal/Multiplicity.hs" #-}
      (meetsMultiplicity (atMost 2) 3)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Multiplicity:98: "
{-# LINE 98 "src/Test/HMock/Internal/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 98 "src/Test/HMock/Internal/Multiplicity.hs" #-}
      (meetsMultiplicity (interval 2 3) 1)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Multiplicity:100: "
{-# LINE 100 "src/Test/HMock/Internal/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 100 "src/Test/HMock/Internal/Multiplicity.hs" #-}
      (meetsMultiplicity (interval 2 3) 2)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Multiplicity:102: "
{-# LINE 102 "src/Test/HMock/Internal/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 102 "src/Test/HMock/Internal/Multiplicity.hs" #-}
      (meetsMultiplicity (interval 2 3) 3)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Multiplicity:104: "
{-# LINE 104 "src/Test/HMock/Internal/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 104 "src/Test/HMock/Internal/Multiplicity.hs" #-}
      (meetsMultiplicity (interval 2 3) 4)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Multiplicity:111: "
{-# LINE 111 "src/Test/HMock/Internal/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 111 "src/Test/HMock/Internal/Multiplicity.hs" #-}
      (exhaustable anyMultiplicity)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Multiplicity:113: "
{-# LINE 113 "src/Test/HMock/Internal/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 113 "src/Test/HMock/Internal/Multiplicity.hs" #-}
      (exhaustable (atLeast 2))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Multiplicity:115: "
{-# LINE 115 "src/Test/HMock/Internal/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 115 "src/Test/HMock/Internal/Multiplicity.hs" #-}
      (exhaustable (atMost 3))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Multiplicity:117: "
{-# LINE 117 "src/Test/HMock/Internal/Multiplicity.hs" #-}
 DocTest.example
{-# LINE 117 "src/Test/HMock/Internal/Multiplicity.hs" #-}
      (exhaustable (interval 0 2))
  [ExpectedLine [LineChunk "True"]]
