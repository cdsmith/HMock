-- Do not edit! Automatically created with doctest-extract from src/Test/HMock/Internal/Predicates.hs
{-# LINE 21 "src/Test/HMock/Internal/Predicates.hs" #-}

{-# OPTIONS_GHC -XTemplateHaskell #-}
{-# OPTIONS_GHC -XTypeApplications #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module DocTests.Test.HMock.Internal.Predicates where

import Test.HMock.Internal.Predicates
import Test.DocTest.Base
import qualified Test.DocTest.Driver as DocTest

{-# LINE 25 "src/Test/HMock/Internal/Predicates.hs" #-}

test :: DocTest.T ()
test = do
 DocTest.printPrefix "Test.HMock.Internal.Predicates:41: "
{-# LINE 41 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 41 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept anything "foo")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:43: "
{-# LINE 43 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 43 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept anything undefined)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:54: "
{-# LINE 54 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 54 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (eq "foo") "foo")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:56: "
{-# LINE 56 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 56 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (eq "foo") "bar")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:67: "
{-# LINE 67 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 67 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (neq "foo") "foo")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:69: "
{-# LINE 69 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 69 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (neq "foo") "bar")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:80: "
{-# LINE 80 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 80 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (gt 5) 4)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:82: "
{-# LINE 82 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 82 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (gt 5) 5)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:84: "
{-# LINE 84 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 84 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (gt 5) 6)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:96: "
{-# LINE 96 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 96 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (geq 5) 4)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:98: "
{-# LINE 98 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 98 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (geq 5) 5)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:100: "
{-# LINE 100 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 100 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (geq 5) 6)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:111: "
{-# LINE 111 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 111 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (lt 5) 4)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:113: "
{-# LINE 113 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 113 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (lt 5) 5)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:115: "
{-# LINE 115 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 115 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (lt 5) 6)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:126: "
{-# LINE 126 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 126 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (leq 5) 4)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:128: "
{-# LINE 128 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 128 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (leq 5) 5)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:130: "
{-# LINE 130 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 130 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (leq 5) 6)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:142: "
{-# LINE 142 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 142 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (just (eq "value")) Nothing)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:144: "
{-# LINE 144 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 144 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (just (eq "value")) (Just "value"))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:146: "
{-# LINE 146 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 146 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (just (eq "value")) (Just "wrong value"))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:158: "
{-# LINE 158 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 158 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (left (eq "value")) (Left "value"))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:160: "
{-# LINE 160 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 160 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (left (eq "value")) (Right "value"))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:162: "
{-# LINE 162 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 162 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (left (eq "value")) (Left "wrong value"))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:174: "
{-# LINE 174 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 174 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (right (eq "value")) (Right "value"))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:176: "
{-# LINE 176 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 176 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (right (eq "value")) (Right "wrong value"))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:178: "
{-# LINE 178 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 178 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (right (eq "value")) (Left "value"))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:189: "
{-# LINE 189 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 189 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (lt "foo" `andP` gt "bar") "eta")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:191: "
{-# LINE 191 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 191 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (lt "foo" `andP` gt "bar") "quz")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:193: "
{-# LINE 193 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 193 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (lt "foo" `andP` gt "bar") "alpha")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:204: "
{-# LINE 204 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 204 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (lt "bar" `orP` gt "foo") "eta")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:206: "
{-# LINE 206 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 206 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (lt "bar" `orP` gt "foo") "quz")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:208: "
{-# LINE 208 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 208 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (lt "bar" `orP` gt "foo") "alpha")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:220: "
{-# LINE 220 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 220 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (notP (eq "negative")) "positive")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:222: "
{-# LINE 222 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 222 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (notP (eq "negative")) "negative")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:233: "
{-# LINE 233 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 233 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (startsWith "fun") "fungible")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:235: "
{-# LINE 235 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 235 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (startsWith "gib") "fungible")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:246: "
{-# LINE 246 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 246 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (endsWith "ow") "crossbow")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:248: "
{-# LINE 248 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 248 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (endsWith "ow") "trebuchet")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:260: "
{-# LINE 260 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 260 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (hasSubstr "i") "team")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:262: "
{-# LINE 262 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 262 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (hasSubstr "i") "partnership")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:274: "
{-# LINE 274 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 274 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (hasSubsequence [1..5]) [1, 2, 3, 4, 5])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:276: "
{-# LINE 276 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 276 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (hasSubsequence [1..5]) [0, 1, 0, 2, 0, 3, 0, 4, 0, 5, 0])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:278: "
{-# LINE 278 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 278 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (hasSubsequence [1..5]) [2, 3, 5, 7, 11])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:297: "
{-# LINE 297 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 297 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (caseInsensitive startsWith "foo") "FOOTBALL!")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:299: "
{-# LINE 299 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 299 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (caseInsensitive endsWith "ball") "soccer")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:301: "
{-# LINE 301 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 301 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (caseInsensitive eq "time") "TIME")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:303: "
{-# LINE 303 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 303 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (caseInsensitive gt "NOTHING") "everything")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:319: "
{-# LINE 319 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 319 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept isEmpty [])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:321: "
{-# LINE 321 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 321 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept isEmpty [1, 2, 3])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:323: "
{-# LINE 323 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 323 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept isEmpty "")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:325: "
{-# LINE 325 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 325 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept isEmpty "gas tank")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:336: "
{-# LINE 336 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 336 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept nonEmpty [])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:338: "
{-# LINE 338 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 338 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept nonEmpty [1, 2, 3])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:340: "
{-# LINE 340 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 340 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept nonEmpty "")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:342: "
{-# LINE 342 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 342 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept nonEmpty "gas tank")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:354: "
{-# LINE 354 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 354 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (sizeIs (lt 3)) ['a' .. 'f'])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:356: "
{-# LINE 356 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 356 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (sizeIs (lt 3)) ['a' .. 'b'])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:368: "
{-# LINE 368 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 368 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (elemsAre [lt 3, lt 4, lt 5]) [2, 3, 4])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:370: "
{-# LINE 370 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 370 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (elemsAre [lt 3, lt 4, lt 5]) [2, 3, 4, 5])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:372: "
{-# LINE 372 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 372 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (elemsAre [lt 3, lt 4, lt 5]) [2, 10, 4])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:387: "
{-# LINE 387 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 387 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (unorderedElemsAre [eq 1, eq 2, eq 3]) [1, 2, 3])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:389: "
{-# LINE 389 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 389 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (unorderedElemsAre [eq 1, eq 2, eq 3]) [2, 3, 1])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:391: "
{-# LINE 391 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 391 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (unorderedElemsAre [eq 1, eq 2, eq 3]) [1, 2, 3, 4])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:393: "
{-# LINE 393 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 393 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (unorderedElemsAre [eq 1, eq 2, eq 3]) [1, 3])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:409: "
{-# LINE 409 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 409 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (each (gt 5)) [4, 5, 6])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:411: "
{-# LINE 411 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 411 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (each (gt 5)) [6, 7, 8])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:413: "
{-# LINE 413 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 413 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (each (gt 5)) [])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:425: "
{-# LINE 425 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 425 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (contains (gt 5)) [3, 4, 5])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:427: "
{-# LINE 427 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 427 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (contains (gt 5)) [4, 5, 6])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:429: "
{-# LINE 429 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 429 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (contains (gt 5)) [])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:443: "
{-# LINE 443 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 443 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (containsAll [eq "foo", eq "bar"]) ["bar", "foo"])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:445: "
{-# LINE 445 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 445 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (containsAll [eq "foo", eq "bar"]) ["foo"])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:447: "
{-# LINE 447 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 447 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (containsAll [eq "foo", eq "bar"]) ["foo", "bar", "qux"])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:460: "
{-# LINE 460 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 460 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (containsOnly [eq "foo", eq "bar"]) ["foo", "foo"])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:462: "
{-# LINE 462 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 462 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (containsOnly [eq "foo", eq "bar"]) ["foo", "bar"])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:464: "
{-# LINE 464 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 464 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (containsOnly [eq "foo", eq "bar"]) ["foo", "qux"])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:480: "
{-# LINE 480 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 480 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (eq 1.0) (sum (replicate 100 0.01)))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:487: "
{-# LINE 487 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 487 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (approxEq 1.0) (sum (replicate 100 0.01)))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:489: "
{-# LINE 489 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 489 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (approxEq 1.0) (sum (replicate 100 0.009999)))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:502: "
{-# LINE 502 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 502 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept finite 1.0)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:504: "
{-# LINE 504 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 504 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept finite (0 / 0))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:506: "
{-# LINE 506 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 506 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept finite (1 / 0))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:517: "
{-# LINE 517 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 517 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept infinite 1.0)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:519: "
{-# LINE 519 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 519 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept infinite (0 / 0))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:521: "
{-# LINE 521 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 521 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept infinite (1 / 0))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:532: "
{-# LINE 532 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 532 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept nAn 1.0)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:534: "
{-# LINE 534 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 534 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept nAn (0 / 0))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:536: "
{-# LINE 536 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 536 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept nAn (1 / 0))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:549: "
{-# LINE 549 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 549 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (suchThat even) 3)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:551: "
{-# LINE 551 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 551 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (suchThat even) 4)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:563: "
{-# LINE 563 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 563 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept $(match [p| Just (Left _) |]) Nothing)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:565: "
{-# LINE 565 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 565 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept $(match [p| Just (Left _) |]) (Just (Left 5)))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:567: "
{-# LINE 567 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 567 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept $(match [p| Just (Left _) |]) (Just (Right 5)))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:583: "
{-# LINE 583 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 583 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (typed @String anything) "foo")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:585: "
{-# LINE 585 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 585 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (typed @String (sizeIs (gt 5))) "foo")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:587: "
{-# LINE 587 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 587 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (typed @String anything) (42 :: Int))
  [ExpectedLine [LineChunk "False"]]
