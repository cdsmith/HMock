-- Do not edit! Automatically created with doctest-extract from src/Test/HMock/Internal/Predicates.hs
{-# LINE 22 "src/Test/HMock/Internal/Predicates.hs" #-}

{-# OPTIONS_GHC -XTemplateHaskell #-}
{-# OPTIONS_GHC -XTypeApplications #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module DocTests.Test.HMock.Internal.Predicates where

import Test.HMock.Internal.Predicates
import Test.DocTest.Base
import qualified Test.DocTest.Driver as DocTest

{-# LINE 26 "src/Test/HMock/Internal/Predicates.hs" #-}

test :: DocTest.T ()
test = do
 DocTest.printPrefix "Test.HMock.Internal.Predicates:42: "
{-# LINE 42 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 42 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept anything "foo")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:44: "
{-# LINE 44 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 44 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept anything undefined)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:55: "
{-# LINE 55 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 55 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (eq "foo") "foo")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:57: "
{-# LINE 57 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 57 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (eq "foo") "bar")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:68: "
{-# LINE 68 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 68 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (neq "foo") "foo")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:70: "
{-# LINE 70 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 70 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (neq "foo") "bar")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:81: "
{-# LINE 81 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 81 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (gt 5) 4)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:83: "
{-# LINE 83 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 83 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (gt 5) 5)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:85: "
{-# LINE 85 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 85 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (gt 5) 6)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:97: "
{-# LINE 97 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 97 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (geq 5) 4)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:99: "
{-# LINE 99 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 99 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (geq 5) 5)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:101: "
{-# LINE 101 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 101 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (geq 5) 6)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:112: "
{-# LINE 112 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 112 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (lt 5) 4)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:114: "
{-# LINE 114 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 114 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (lt 5) 5)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:116: "
{-# LINE 116 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 116 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (lt 5) 6)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:127: "
{-# LINE 127 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 127 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (leq 5) 4)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:129: "
{-# LINE 129 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 129 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (leq 5) 5)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:131: "
{-# LINE 131 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 131 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (leq 5) 6)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:143: "
{-# LINE 143 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 143 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (just (eq "value")) Nothing)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:145: "
{-# LINE 145 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 145 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (just (eq "value")) (Just "value"))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:147: "
{-# LINE 147 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 147 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (just (eq "value")) (Just "wrong value"))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:159: "
{-# LINE 159 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 159 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (left (eq "value")) (Left "value"))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:161: "
{-# LINE 161 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 161 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (left (eq "value")) (Right "value"))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:163: "
{-# LINE 163 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 163 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (left (eq "value")) (Left "wrong value"))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:175: "
{-# LINE 175 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 175 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (right (eq "value")) (Right "value"))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:177: "
{-# LINE 177 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 177 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (right (eq "value")) (Right "wrong value"))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:179: "
{-# LINE 179 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 179 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (right (eq "value")) (Left "value"))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:190: "
{-# LINE 190 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 190 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (lt "foo" `andP` gt "bar") "eta")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:192: "
{-# LINE 192 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 192 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (lt "foo" `andP` gt "bar") "quz")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:194: "
{-# LINE 194 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 194 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (lt "foo" `andP` gt "bar") "alpha")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:205: "
{-# LINE 205 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 205 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (lt "bar" `orP` gt "foo") "eta")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:207: "
{-# LINE 207 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 207 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (lt "bar" `orP` gt "foo") "quz")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:209: "
{-# LINE 209 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 209 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (lt "bar" `orP` gt "foo") "alpha")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:221: "
{-# LINE 221 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 221 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (notP (eq "negative")) "positive")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:223: "
{-# LINE 223 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 223 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (notP (eq "negative")) "negative")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:234: "
{-# LINE 234 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 234 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (startsWith "fun") "fungible")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:236: "
{-# LINE 236 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 236 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (startsWith "gib") "fungible")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:247: "
{-# LINE 247 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 247 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (endsWith "ow") "crossbow")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:249: "
{-# LINE 249 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 249 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (endsWith "ow") "trebuchet")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:261: "
{-# LINE 261 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 261 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (hasSubstr "i") "team")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:263: "
{-# LINE 263 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 263 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (hasSubstr "i") "partnership")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:275: "
{-# LINE 275 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 275 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (hasSubsequence [1..5]) [1, 2, 3, 4, 5])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:277: "
{-# LINE 277 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 277 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (hasSubsequence [1..5]) [0, 1, 0, 2, 0, 3, 0, 4, 0, 5, 0])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:279: "
{-# LINE 279 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 279 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (hasSubsequence [1..5]) [2, 3, 5, 7, 11])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:291: "
{-# LINE 291 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 291 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (caseInsensitive startsWith "foo") "FOOTBALL!")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:293: "
{-# LINE 293 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 293 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (caseInsensitive endsWith "ball") "soccer")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:295: "
{-# LINE 295 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 295 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (caseInsensitive eq "time") "TIME")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:297: "
{-# LINE 297 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 297 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (caseInsensitive gt "NOTHING") "everything")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:313: "
{-# LINE 313 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 313 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept isEmpty [])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:315: "
{-# LINE 315 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 315 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept isEmpty [1, 2, 3])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:317: "
{-# LINE 317 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 317 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept isEmpty "")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:319: "
{-# LINE 319 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 319 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept isEmpty "gas tank")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:330: "
{-# LINE 330 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 330 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept nonEmpty [])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:332: "
{-# LINE 332 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 332 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept nonEmpty [1, 2, 3])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:334: "
{-# LINE 334 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 334 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept nonEmpty "")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:336: "
{-# LINE 336 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 336 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept nonEmpty "gas tank")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:348: "
{-# LINE 348 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 348 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (sizeIs (lt 3)) ['a' .. 'f'])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:350: "
{-# LINE 350 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 350 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (sizeIs (lt 3)) ['a' .. 'b'])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:362: "
{-# LINE 362 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 362 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (elemsAre [lt 3, lt 4, lt 5]) [2, 3, 4])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:364: "
{-# LINE 364 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 364 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (elemsAre [lt 3, lt 4, lt 5]) [2, 3, 4, 5])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:366: "
{-# LINE 366 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 366 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (elemsAre [lt 3, lt 4, lt 5]) [2, 10, 4])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:380: "
{-# LINE 380 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 380 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (unorderedElemsAre [eq 1, eq 2, eq 3]) [1, 2, 3])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:382: "
{-# LINE 382 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 382 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (unorderedElemsAre [eq 1, eq 2, eq 3]) [2, 3, 1])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:384: "
{-# LINE 384 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 384 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (unorderedElemsAre [eq 1, eq 2, eq 3]) [1, 2, 3, 4])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:386: "
{-# LINE 386 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 386 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (unorderedElemsAre [eq 1, eq 2, eq 3]) [1, 3])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:402: "
{-# LINE 402 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 402 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (each (gt 5)) [4, 5, 6])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:404: "
{-# LINE 404 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 404 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (each (gt 5)) [6, 7, 8])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:406: "
{-# LINE 406 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 406 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (each (gt 5)) [])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:418: "
{-# LINE 418 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 418 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (contains (gt 5)) [3, 4, 5])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:420: "
{-# LINE 420 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 420 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (contains (gt 5)) [4, 5, 6])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:422: "
{-# LINE 422 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 422 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (contains (gt 5)) [])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:436: "
{-# LINE 436 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 436 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (containsAll [eq "foo", eq "bar"]) ["bar", "foo"])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:438: "
{-# LINE 438 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 438 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (containsAll [eq "foo", eq "bar"]) ["foo"])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:440: "
{-# LINE 440 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 440 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (containsAll [eq "foo", eq "bar"]) ["foo", "bar", "qux"])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:453: "
{-# LINE 453 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 453 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (containsOnly [eq "foo", eq "bar"]) ["foo", "foo"])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:455: "
{-# LINE 455 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 455 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (containsOnly [eq "foo", eq "bar"]) ["foo", "bar"])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:457: "
{-# LINE 457 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 457 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (containsOnly [eq "foo", eq "bar"]) ["foo", "qux"])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:469: "
{-# LINE 469 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 469 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (containsKey (eq "foo")) [("foo", 1), ("bar", 2)])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:471: "
{-# LINE 471 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 471 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (containsKey (eq "foo")) [("bar", 1), ("qux", 2)])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:484: "
{-# LINE 484 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 484 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (containsEntry (eq "foo") (gt 10)) [("foo", 12), ("bar", 5)])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:486: "
{-# LINE 486 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 486 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (containsEntry (eq "foo") (gt 10)) [("foo", 5), ("bar", 12)])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:488: "
{-# LINE 488 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 488 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (containsEntry (eq "foo") (gt 10)) [("bar", 12)])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:501: "
{-# LINE 501 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 501 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (keysAre [eq "a", eq "b", eq "c"]) [("a", 1), ("b", 2), ("c", 3)])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:503: "
{-# LINE 503 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 503 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (keysAre [eq "a", eq "b", eq "c"]) [("c", 1), ("b", 2), ("a", 3)])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:505: "
{-# LINE 505 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 505 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (keysAre [eq "a", eq "b", eq "c"]) [("a", 1), ("c", 3)])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:507: "
{-# LINE 507 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 507 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (keysAre [eq "a", eq "b"]) [("a", 1), ("b", 2), ("c", 3)])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:523: "
{-# LINE 523 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 523 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (entriesAre [(eq 1, eq 2), (eq 3, eq 4)]) [(1, 2), (3, 4)])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:525: "
{-# LINE 525 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 525 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (entriesAre [(eq 1, eq 2), (eq 3, eq 4)]) [(3, 4), (1, 2)])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:527: "
{-# LINE 527 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 527 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (entriesAre [(eq 1, eq 2), (eq 3, eq 4)]) [(1, 4), (3, 2)])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:529: "
{-# LINE 529 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 529 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (entriesAre [(eq 1, eq 2), (eq 3, eq 4)]) [(1, 2), (3, 4), (5, 6)])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:550: "
{-# LINE 550 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 550 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (eq 1.0) (sum (replicate 100 0.01)))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:557: "
{-# LINE 557 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 557 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (approxEq 1.0) (sum (replicate 100 0.01)))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:559: "
{-# LINE 559 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 559 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (approxEq 1.0) (sum (replicate 100 0.009999)))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:572: "
{-# LINE 572 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 572 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept finite 1.0)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:574: "
{-# LINE 574 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 574 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept finite (0 / 0))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:576: "
{-# LINE 576 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 576 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept finite (1 / 0))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:587: "
{-# LINE 587 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 587 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept infinite 1.0)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:589: "
{-# LINE 589 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 589 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept infinite (0 / 0))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:591: "
{-# LINE 591 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 591 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept infinite (1 / 0))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:602: "
{-# LINE 602 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 602 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept nAn 1.0)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:604: "
{-# LINE 604 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 604 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept nAn (0 / 0))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:606: "
{-# LINE 606 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 606 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept nAn (1 / 0))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:619: "
{-# LINE 619 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 619 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (suchThat even) 3)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:621: "
{-# LINE 621 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 621 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (suchThat even) 4)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:633: "
{-# LINE 633 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 633 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept $(match [p| Just (Left _) |]) Nothing)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:635: "
{-# LINE 635 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 635 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept $(match [p| Just (Left _) |]) (Just (Left 5)))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:637: "
{-# LINE 637 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 637 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept $(match [p| Just (Left _) |]) (Just (Right 5)))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:653: "
{-# LINE 653 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 653 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (typed @String anything) "foo")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:655: "
{-# LINE 655 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 655 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (typed @String (sizeIs (gt 5))) "foo")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:657: "
{-# LINE 657 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 657 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (typed @String anything) (42 :: Int))
  [ExpectedLine [LineChunk "False"]]
