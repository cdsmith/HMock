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
      (accept (just (eq "value")) Nothing)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:143: "
{-# LINE 143 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 143 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (just (eq "value")) (Just "value"))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:145: "
{-# LINE 145 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 145 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (just (eq "value")) (Just "wrong value"))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:157: "
{-# LINE 157 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 157 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (left (eq "value")) (Left "value"))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:159: "
{-# LINE 159 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 159 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (left (eq "value")) (Right "value"))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:161: "
{-# LINE 161 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 161 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (left (eq "value")) (Left "wrong value"))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:173: "
{-# LINE 173 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 173 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (right (eq "value")) (Right "value"))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:175: "
{-# LINE 175 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 175 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (right (eq "value")) (Right "wrong value"))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:177: "
{-# LINE 177 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 177 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (right (eq "value")) (Left "value"))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:188: "
{-# LINE 188 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 188 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (lt "foo" `andP` gt "bar") "eta")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:190: "
{-# LINE 190 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 190 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (lt "foo" `andP` gt "bar") "quz")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:192: "
{-# LINE 192 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 192 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (lt "foo" `andP` gt "bar") "alpha")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:203: "
{-# LINE 203 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 203 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (lt "bar" `orP` gt "foo") "eta")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:205: "
{-# LINE 205 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 205 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (lt "bar" `orP` gt "foo") "quz")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:207: "
{-# LINE 207 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 207 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (lt "bar" `orP` gt "foo") "alpha")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:219: "
{-# LINE 219 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 219 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (notP (eq "negative")) "positive")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:221: "
{-# LINE 221 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 221 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (notP (eq "negative")) "negative")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:232: "
{-# LINE 232 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 232 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (startsWith "fun") "fungible")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:234: "
{-# LINE 234 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 234 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (startsWith "gib") "fungible")
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
      (accept (hasSubsequence [1..5]) [2, 3, 5, 7, 11])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:296: "
{-# LINE 296 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 296 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (caseInsensitive startsWith "foo") "FOOTBALL!")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:298: "
{-# LINE 298 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 298 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (caseInsensitive endsWith "ball") "soccer")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:300: "
{-# LINE 300 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 300 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (caseInsensitive eq "time") "TIME")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:302: "
{-# LINE 302 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 302 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (caseInsensitive gt "NOTHING") "everything")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:318: "
{-# LINE 318 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 318 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept isEmpty [])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:320: "
{-# LINE 320 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 320 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept isEmpty [1, 2, 3])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:322: "
{-# LINE 322 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 322 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept isEmpty "")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:324: "
{-# LINE 324 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 324 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept isEmpty "gas tank")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:335: "
{-# LINE 335 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 335 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept nonEmpty [])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:337: "
{-# LINE 337 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 337 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept nonEmpty [1, 2, 3])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:339: "
{-# LINE 339 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 339 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept nonEmpty "")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:341: "
{-# LINE 341 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 341 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept nonEmpty "gas tank")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:353: "
{-# LINE 353 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 353 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (sizeIs (lt 3)) ['a' .. 'f'])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:355: "
{-# LINE 355 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 355 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (sizeIs (lt 3)) ['a' .. 'b'])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:367: "
{-# LINE 367 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 367 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (elemsAre [lt 3, lt 4, lt 5]) [2, 3, 4])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:369: "
{-# LINE 369 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 369 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (elemsAre [lt 3, lt 4, lt 5]) [2, 3, 4, 5])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:371: "
{-# LINE 371 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 371 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (elemsAre [lt 3, lt 4, lt 5]) [2, 10, 4])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:386: "
{-# LINE 386 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 386 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (unorderedElemsAre [eq 1, eq 2, eq 3]) [1, 2, 3])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:388: "
{-# LINE 388 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 388 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (unorderedElemsAre [eq 1, eq 2, eq 3]) [2, 3, 1])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:390: "
{-# LINE 390 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 390 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (unorderedElemsAre [eq 1, eq 2, eq 3]) [1, 2, 3, 4])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:392: "
{-# LINE 392 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 392 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (unorderedElemsAre [eq 1, eq 2, eq 3]) [1, 3])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:408: "
{-# LINE 408 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 408 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (each (gt 5)) [4, 5, 6])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:410: "
{-# LINE 410 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 410 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (each (gt 5)) [6, 7, 8])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:412: "
{-# LINE 412 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 412 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (each (gt 5)) [])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:424: "
{-# LINE 424 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 424 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (contains (gt 5)) [3, 4, 5])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:426: "
{-# LINE 426 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 426 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (contains (gt 5)) [4, 5, 6])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:428: "
{-# LINE 428 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 428 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (contains (gt 5)) [])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:442: "
{-# LINE 442 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 442 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (containsAll [eq "foo", eq "bar"]) ["bar", "foo"])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:444: "
{-# LINE 444 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 444 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (containsAll [eq "foo", eq "bar"]) ["foo"])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:446: "
{-# LINE 446 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 446 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (containsAll [eq "foo", eq "bar"]) ["foo", "bar", "qux"])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:459: "
{-# LINE 459 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 459 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (containsOnly [eq "foo", eq "bar"]) ["foo", "foo"])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:461: "
{-# LINE 461 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 461 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (containsOnly [eq "foo", eq "bar"]) ["foo", "bar"])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:463: "
{-# LINE 463 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 463 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (containsOnly [eq "foo", eq "bar"]) ["foo", "qux"])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:479: "
{-# LINE 479 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 479 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (eq 1.0) (sum (replicate 100 0.01)))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:486: "
{-# LINE 486 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 486 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (approxEq 1.0) (sum (replicate 100 0.01)))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:488: "
{-# LINE 488 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 488 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (approxEq 1.0) (sum (replicate 100 0.009999)))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:501: "
{-# LINE 501 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 501 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept finite 1.0)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:503: "
{-# LINE 503 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 503 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept finite (0 / 0))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:505: "
{-# LINE 505 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 505 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept finite (1 / 0))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:516: "
{-# LINE 516 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 516 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept infinite 1.0)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:518: "
{-# LINE 518 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 518 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept infinite (0 / 0))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:520: "
{-# LINE 520 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 520 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept infinite (1 / 0))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:531: "
{-# LINE 531 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 531 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept nAn 1.0)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:533: "
{-# LINE 533 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 533 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept nAn (0 / 0))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:535: "
{-# LINE 535 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 535 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept nAn (1 / 0))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:548: "
{-# LINE 548 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 548 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (suchThat even) 3)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:550: "
{-# LINE 550 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 550 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (suchThat even) 4)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:562: "
{-# LINE 562 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 562 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept $(match [p| Just (Left _) |]) Nothing)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:564: "
{-# LINE 564 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 564 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept $(match [p| Just (Left _) |]) (Just (Left 5)))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:566: "
{-# LINE 566 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 566 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept $(match [p| Just (Left _) |]) (Just (Right 5)))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:582: "
{-# LINE 582 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 582 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (typed @String anything) "foo")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:584: "
{-# LINE 584 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 584 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (typed @String (sizeIs (gt 5))) "foo")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:586: "
{-# LINE 586 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 586 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (typed @String anything) (42 :: Int))
  [ExpectedLine [LineChunk "False"]]
