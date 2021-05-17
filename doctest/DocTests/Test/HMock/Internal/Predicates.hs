-- Do not edit! Automatically created with doctest-extract from src/Test/HMock/Internal/Predicates.hs
{-# LINE 21 "src/Test/HMock/Internal/Predicates.hs" #-}

{-# OPTIONS_GHC -XTemplateHaskell #-}
{-# OPTIONS_GHC -XTypeApplications #-}
module DocTests.Test.HMock.Internal.Predicates where

import Test.HMock.Internal.Predicates
import Test.DocTest.Base
import qualified Test.DocTest.Driver as DocTest

{-# LINE 24 "src/Test/HMock/Internal/Predicates.hs" #-}

test :: DocTest.T ()
test = do
 DocTest.printPrefix "Test.HMock.Internal.Predicates:40: "
{-# LINE 40 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 40 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept anything "foo")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:42: "
{-# LINE 42 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 42 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept anything undefined)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:53: "
{-# LINE 53 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 53 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (eq "foo") "foo")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:55: "
{-# LINE 55 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 55 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (eq "foo") "bar")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:66: "
{-# LINE 66 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 66 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (neq "foo") "foo")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:68: "
{-# LINE 68 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 68 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (neq "foo") "bar")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:79: "
{-# LINE 79 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 79 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (gt 5) 4)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:81: "
{-# LINE 81 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 81 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (gt 5) 5)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:83: "
{-# LINE 83 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 83 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (gt 5) 6)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:95: "
{-# LINE 95 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 95 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (geq 5) 4)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:97: "
{-# LINE 97 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 97 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (geq 5) 5)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:99: "
{-# LINE 99 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 99 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (geq 5) 6)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:110: "
{-# LINE 110 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 110 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (lt 5) 4)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:112: "
{-# LINE 112 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 112 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (lt 5) 5)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:114: "
{-# LINE 114 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 114 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (lt 5) 6)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:125: "
{-# LINE 125 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 125 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (leq 5) 4)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:127: "
{-# LINE 127 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 127 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (leq 5) 5)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:129: "
{-# LINE 129 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 129 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (leq 5) 6)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:141: "
{-# LINE 141 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 141 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (just (lt 5)) Nothing)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:143: "
{-# LINE 143 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 143 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (just (lt 5)) (Just 10))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:145: "
{-# LINE 145 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 145 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (just (lt 5)) (Just 4))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:157: "
{-# LINE 157 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 157 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (left (lt 5)) (Left 4))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:159: "
{-# LINE 159 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 159 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (left (lt 5)) (Left 5))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:161: "
{-# LINE 161 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 161 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (left (lt 5)) (Right 4))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:173: "
{-# LINE 173 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 173 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (right (lt 5)) (Left 4))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:175: "
{-# LINE 175 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 175 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (right (lt 5)) (Right 4))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:177: "
{-# LINE 177 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 177 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (right (lt 5)) (Right 5))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:188: "
{-# LINE 188 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 188 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (lt 5 `andP` gt 3) 3)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:190: "
{-# LINE 190 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 190 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (lt 5 `andP` gt 3) 4)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:192: "
{-# LINE 192 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 192 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (lt 5 `andP` gt 3) 5)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:203: "
{-# LINE 203 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 203 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (lt 4 `orP` gt 5) 3)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:205: "
{-# LINE 205 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 205 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (lt 4 `orP` gt 5) 4)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:207: "
{-# LINE 207 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 207 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (lt 4 `orP` gt 5) 6)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:219: "
{-# LINE 219 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 219 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (notP (eq 5)) 4)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:221: "
{-# LINE 221 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 221 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (notP (eq 5)) 5)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:232: "
{-# LINE 232 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 232 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (startsWith "foo") "football")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:234: "
{-# LINE 234 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 234 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (startsWith "foo") "soccer")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:245: "
{-# LINE 245 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 245 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (endsWith "ow") "crossbow")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:247: "
{-# LINE 247 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 247 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (endsWith "ow") "trebuchet")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:259: "
{-# LINE 259 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 259 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (hasSubstr "i") "team")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:261: "
{-# LINE 261 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 261 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (hasSubstr "i") "partnership")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:273: "
{-# LINE 273 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 273 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (hasSubsequence [1..5]) [1, 2, 3, 4, 5])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:275: "
{-# LINE 275 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 275 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (hasSubsequence [1..5]) [0, 1, 0, 2, 0, 3, 0, 4, 0, 5, 0])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:277: "
{-# LINE 277 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 277 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (hasSubsequence [1..5]) [5, 4, 3, 2, 1])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:296: "
{-# LINE 296 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 296 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (caseInsensitive startsWith "foo") "foot")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:298: "
{-# LINE 298 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 298 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (caseInsensitive startsWith "foo") "FOOT")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:300: "
{-# LINE 300 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 300 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (caseInsensitive startsWith "foo") "bar")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:316: "
{-# LINE 316 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 316 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept isEmpty [])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:318: "
{-# LINE 318 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 318 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept isEmpty [1, 2, 3])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:329: "
{-# LINE 329 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 329 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept nonEmpty [])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:331: "
{-# LINE 331 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 331 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept nonEmpty [1, 2, 3])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:343: "
{-# LINE 343 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 343 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (sizeIs (lt 3)) ['a' .. 'f'])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:345: "
{-# LINE 345 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 345 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (sizeIs (lt 3)) ['a' .. 'b'])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:357: "
{-# LINE 357 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 357 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (elemsAre [lt 3, gt 4, lt 5]) [])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:359: "
{-# LINE 359 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 359 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (elemsAre [lt 3, lt 4, lt 5]) [2, 3, 4])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:361: "
{-# LINE 361 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 361 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (elemsAre [lt 3, lt 4, lt 5]) [2, 10, 4])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:376: "
{-# LINE 376 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 376 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (unorderedElemsAre [lt 4, gt 10]) [2, 11])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:378: "
{-# LINE 378 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 378 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (unorderedElemsAre [lt 4, gt 10]) [11, 2])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:380: "
{-# LINE 380 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 380 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (unorderedElemsAre [lt 4, gt 10]) [2, 2])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:382: "
{-# LINE 382 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 382 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (unorderedElemsAre [lt 4, gt 10]) [2])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:398: "
{-# LINE 398 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 398 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (each (gt 5)) [4, 5, 6])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:400: "
{-# LINE 400 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 400 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (each (gt 5)) [6, 7, 8])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:402: "
{-# LINE 402 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 402 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (each (gt 5)) [])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:414: "
{-# LINE 414 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 414 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (contains (gt 5)) [3, 4, 5])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:416: "
{-# LINE 416 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 416 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (contains (gt 5)) [4, 5, 6])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:418: "
{-# LINE 418 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 418 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (contains (gt 5)) [])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:432: "
{-# LINE 432 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 432 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (containsAll [eq "foo", caseInsensitive eq "bar"]) ["foo", "BAR"])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:434: "
{-# LINE 434 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 434 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (containsAll [eq "foo", caseInsensitive eq "bar"]) ["foo"])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:447: "
{-# LINE 447 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 447 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (containsOnly [eq "foo", lt "bar"]) ["foo", "alpha"])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:449: "
{-# LINE 449 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 449 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (containsOnly [eq "foo", lt "bar"]) ["foo", "alpha", "qux"])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:465: "
{-# LINE 465 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 465 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (eq 1.0) (sum (replicate 100 0.01)))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:472: "
{-# LINE 472 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 472 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (approxEq 1.0) (sum (replicate 100 0.01)))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:474: "
{-# LINE 474 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 474 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (approxEq 1.0) (sum (replicate 100 0.009999)))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:487: "
{-# LINE 487 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 487 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept finite 1.0)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:489: "
{-# LINE 489 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 489 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept finite (0 / 0))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:491: "
{-# LINE 491 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 491 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept finite (1 / 0))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:502: "
{-# LINE 502 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 502 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept infinite 1.0)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:504: "
{-# LINE 504 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 504 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept infinite (0 / 0))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:506: "
{-# LINE 506 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 506 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept infinite (1 / 0))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:517: "
{-# LINE 517 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 517 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept nAn 1.0)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:519: "
{-# LINE 519 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 519 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept nAn (0 / 0))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:521: "
{-# LINE 521 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 521 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept nAn (1 / 0))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:534: "
{-# LINE 534 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 534 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (suchThat even) 3)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:536: "
{-# LINE 536 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 536 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (suchThat even) 4)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:548: "
{-# LINE 548 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 548 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept $(match [p| Just (Left _) |]) Nothing)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:550: "
{-# LINE 550 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 550 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept $(match [p| Just (Left _) |]) (Just (Left 5)))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:552: "
{-# LINE 552 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 552 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept $(match [p| Just (Left _) |]) (Just (Right 5)))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:568: "
{-# LINE 568 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 568 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (typed @String anything) "foo")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:570: "
{-# LINE 570 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 570 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (typed @String (sizeIs (gt 5))) "foo")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:572: "
{-# LINE 572 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 572 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (typed @String anything) (42 :: Int))
  [ExpectedLine [LineChunk "False"]]
