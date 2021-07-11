-- Do not edit! Automatically created with doctest-extract from src/Test/HMock/Predicates.hs
{-# LINE 79 "src/Test/HMock/Predicates.hs" #-}

{-# OPTIONS_GHC -XTemplateHaskell #-}
{-# OPTIONS_GHC -XTypeApplications #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module DocTests.Test.HMock.Predicates where

import Test.HMock.Predicates
import Test.DocTest.Base
import qualified Test.DocTest.Driver as DocTest

{-# LINE 83 "src/Test/HMock/Predicates.hs" #-}

test :: DocTest.T ()
test = do
 DocTest.printPrefix "Test.HMock.Predicates:101: "
{-# LINE 101 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 101 "src/Test/HMock/Predicates.hs" #-}
      (accept anything "foo")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:103: "
{-# LINE 103 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 103 "src/Test/HMock/Predicates.hs" #-}
      (accept anything undefined)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:116: "
{-# LINE 116 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 116 "src/Test/HMock/Predicates.hs" #-}
      (accept (eq "foo") "foo")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:118: "
{-# LINE 118 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 118 "src/Test/HMock/Predicates.hs" #-}
      (accept (eq "foo") "bar")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:135: "
{-# LINE 135 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 135 "src/Test/HMock/Predicates.hs" #-}
      (accept (neq "foo") "foo")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:137: "
{-# LINE 137 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 137 "src/Test/HMock/Predicates.hs" #-}
      (accept (neq "foo") "bar")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:144: "
{-# LINE 144 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 144 "src/Test/HMock/Predicates.hs" #-}
      (accept (gt 5) 4)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:146: "
{-# LINE 146 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 146 "src/Test/HMock/Predicates.hs" #-}
      (accept (gt 5) 5)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:148: "
{-# LINE 148 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 148 "src/Test/HMock/Predicates.hs" #-}
      (accept (gt 5) 6)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:166: "
{-# LINE 166 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 166 "src/Test/HMock/Predicates.hs" #-}
      (accept (geq 5) 4)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:168: "
{-# LINE 168 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 168 "src/Test/HMock/Predicates.hs" #-}
      (accept (geq 5) 5)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:170: "
{-# LINE 170 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 170 "src/Test/HMock/Predicates.hs" #-}
      (accept (geq 5) 6)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:187: "
{-# LINE 187 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 187 "src/Test/HMock/Predicates.hs" #-}
      (accept (lt 5) 4)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:189: "
{-# LINE 189 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 189 "src/Test/HMock/Predicates.hs" #-}
      (accept (lt 5) 5)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:191: "
{-# LINE 191 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 191 "src/Test/HMock/Predicates.hs" #-}
      (accept (lt 5) 6)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:198: "
{-# LINE 198 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 198 "src/Test/HMock/Predicates.hs" #-}
      (accept (leq 5) 4)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:200: "
{-# LINE 200 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 200 "src/Test/HMock/Predicates.hs" #-}
      (accept (leq 5) 5)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:202: "
{-# LINE 202 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 202 "src/Test/HMock/Predicates.hs" #-}
      (accept (leq 5) 6)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:210: "
{-# LINE 210 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 210 "src/Test/HMock/Predicates.hs" #-}
      (accept (just (eq "value")) Nothing)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:212: "
{-# LINE 212 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 212 "src/Test/HMock/Predicates.hs" #-}
      (accept (just (eq "value")) (Just "value"))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:214: "
{-# LINE 214 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 214 "src/Test/HMock/Predicates.hs" #-}
      (accept (just (eq "value")) (Just "wrong value"))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:237: "
{-# LINE 237 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 237 "src/Test/HMock/Predicates.hs" #-}
      (accept (left (eq "value")) (Left "value"))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:239: "
{-# LINE 239 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 239 "src/Test/HMock/Predicates.hs" #-}
      (accept (left (eq "value")) (Right "value"))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:241: "
{-# LINE 241 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 241 "src/Test/HMock/Predicates.hs" #-}
      (accept (left (eq "value")) (Left "wrong value"))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:255: "
{-# LINE 255 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 255 "src/Test/HMock/Predicates.hs" #-}
      (accept (right (eq "value")) (Right "value"))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:257: "
{-# LINE 257 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 257 "src/Test/HMock/Predicates.hs" #-}
      (accept (right (eq "value")) (Right "wrong value"))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:259: "
{-# LINE 259 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 259 "src/Test/HMock/Predicates.hs" #-}
      (accept (right (eq "value")) (Left "value"))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:273: "
{-# LINE 273 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 273 "src/Test/HMock/Predicates.hs" #-}
      (accept (zipP (eq "foo") (eq "bar")) ("foo", "bar"))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:275: "
{-# LINE 275 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 275 "src/Test/HMock/Predicates.hs" #-}
      (accept (zipP (eq "foo") (eq "bar")) ("bar", "foo"))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:289: "
{-# LINE 289 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 289 "src/Test/HMock/Predicates.hs" #-}
      (accept (zip3P (eq "foo") (eq "bar") (eq "qux")) ("foo", "bar", "qux"))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:291: "
{-# LINE 291 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 291 "src/Test/HMock/Predicates.hs" #-}
      (accept (zip3P (eq "foo") (eq "bar") (eq "qux")) ("qux", "bar", "foo"))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:305: "
{-# LINE 305 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 305 "src/Test/HMock/Predicates.hs" #-}
      (accept (zip4P (eq 1) (eq 2) (eq 3) (eq 4)) (1, 2, 3, 4))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:307: "
{-# LINE 307 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 307 "src/Test/HMock/Predicates.hs" #-}
      (accept (zip4P (eq 1) (eq 2) (eq 3) (eq 4)) (4, 3, 2, 1))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:327: "
{-# LINE 327 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 327 "src/Test/HMock/Predicates.hs" #-}
      (accept (zip5P (eq 1) (eq 2) (eq 3) (eq 4) (eq 5)) (1, 2, 3, 4, 5))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:329: "
{-# LINE 329 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 329 "src/Test/HMock/Predicates.hs" #-}
      (accept (zip5P (eq 1) (eq 2) (eq 3) (eq 4) (eq 5)) (5, 4, 3, 2, 1))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:350: "
{-# LINE 350 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 350 "src/Test/HMock/Predicates.hs" #-}
      (accept (lt "foo" `andP` gt "bar") "eta")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:352: "
{-# LINE 352 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 352 "src/Test/HMock/Predicates.hs" #-}
      (accept (lt "foo" `andP` gt "bar") "quz")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:354: "
{-# LINE 354 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 354 "src/Test/HMock/Predicates.hs" #-}
      (accept (lt "foo" `andP` gt "bar") "alpha")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:373: "
{-# LINE 373 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 373 "src/Test/HMock/Predicates.hs" #-}
      (accept (lt "bar" `orP` gt "foo") "eta")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:375: "
{-# LINE 375 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 375 "src/Test/HMock/Predicates.hs" #-}
      (accept (lt "bar" `orP` gt "foo") "quz")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:377: "
{-# LINE 377 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 377 "src/Test/HMock/Predicates.hs" #-}
      (accept (lt "bar" `orP` gt "foo") "alpha")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:385: "
{-# LINE 385 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 385 "src/Test/HMock/Predicates.hs" #-}
      (accept (notP (eq "negative")) "positive")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:387: "
{-# LINE 387 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 387 "src/Test/HMock/Predicates.hs" #-}
      (accept (notP (eq "negative")) "negative")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:400: "
{-# LINE 400 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 400 "src/Test/HMock/Predicates.hs" #-}
      (accept (startsWith "fun") "fungible")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:402: "
{-# LINE 402 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 402 "src/Test/HMock/Predicates.hs" #-}
      (accept (startsWith "gib") "fungible")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:415: "
{-# LINE 415 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 415 "src/Test/HMock/Predicates.hs" #-}
      (accept (endsWith "ow") "crossbow")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:417: "
{-# LINE 417 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 417 "src/Test/HMock/Predicates.hs" #-}
      (accept (endsWith "ow") "trebuchet")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:431: "
{-# LINE 431 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 431 "src/Test/HMock/Predicates.hs" #-}
      (accept (hasSubstr "i") "team")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:433: "
{-# LINE 433 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 433 "src/Test/HMock/Predicates.hs" #-}
      (accept (hasSubstr "i") "partnership")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:447: "
{-# LINE 447 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 447 "src/Test/HMock/Predicates.hs" #-}
      (accept (hasSubsequence [1..5]) [1, 2, 3, 4, 5])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:449: "
{-# LINE 449 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 449 "src/Test/HMock/Predicates.hs" #-}
      (accept (hasSubsequence [1..5]) [0, 1, 0, 2, 0, 3, 0, 4, 0, 5, 0])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:451: "
{-# LINE 451 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 451 "src/Test/HMock/Predicates.hs" #-}
      (accept (hasSubsequence [1..5]) [2, 3, 5, 7, 11])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:465: "
{-# LINE 465 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 465 "src/Test/HMock/Predicates.hs" #-}
      (accept (caseInsensitive startsWith "foo") "FOOTBALL!")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:467: "
{-# LINE 467 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 467 "src/Test/HMock/Predicates.hs" #-}
      (accept (caseInsensitive endsWith "ball") "soccer")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:469: "
{-# LINE 469 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 469 "src/Test/HMock/Predicates.hs" #-}
      (accept (caseInsensitive eq "time") "TIME")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:471: "
{-# LINE 471 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 471 "src/Test/HMock/Predicates.hs" #-}
      (accept (caseInsensitive gt "NOTHING") "everything")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:498: "
{-# LINE 498 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 498 "src/Test/HMock/Predicates.hs" #-}
      (accept (matchesRegex "x{2,5}y?") "xxxy")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:500: "
{-# LINE 500 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 500 "src/Test/HMock/Predicates.hs" #-}
      (accept (matchesRegex "x{2,5}y?") "xyy")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:502: "
{-# LINE 502 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 502 "src/Test/HMock/Predicates.hs" #-}
      (accept (matchesRegex "x{2,5}y?") "wxxxyz")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:533: "
{-# LINE 533 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 533 "src/Test/HMock/Predicates.hs" #-}
      (accept (matchesCaseInsensitiveRegex "x{2,5}y?") "XXXY")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:535: "
{-# LINE 535 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 535 "src/Test/HMock/Predicates.hs" #-}
      (accept (matchesCaseInsensitiveRegex "x{2,5}y?") "XYY")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:537: "
{-# LINE 537 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 537 "src/Test/HMock/Predicates.hs" #-}
      (accept (matchesCaseInsensitiveRegex "x{2,5}y?") "WXXXYZ")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:574: "
{-# LINE 574 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 574 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsRegex "x{2,5}y?") "xxxy")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:576: "
{-# LINE 576 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 576 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsRegex "x{2,5}y?") "xyy")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:578: "
{-# LINE 578 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 578 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsRegex "x{2,5}y?") "wxxxyz")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:606: "
{-# LINE 606 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 606 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsCaseInsensitiveRegex "x{2,5}y?") "XXXY")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:608: "
{-# LINE 608 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 608 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsCaseInsensitiveRegex "x{2,5}y?") "XYY")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:610: "
{-# LINE 610 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 610 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsCaseInsensitiveRegex "x{2,5}y?") "WXXXYZ")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:638: "
{-# LINE 638 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 638 "src/Test/HMock/Predicates.hs" #-}
      (accept isEmpty ([] :: [Int]))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:640: "
{-# LINE 640 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 640 "src/Test/HMock/Predicates.hs" #-}
      (accept isEmpty [1, 2, 3])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:642: "
{-# LINE 642 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 642 "src/Test/HMock/Predicates.hs" #-}
      (accept isEmpty "")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:644: "
{-# LINE 644 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 644 "src/Test/HMock/Predicates.hs" #-}
      (accept isEmpty "gas tank")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:661: "
{-# LINE 661 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 661 "src/Test/HMock/Predicates.hs" #-}
      (accept nonEmpty ([] :: [Int]))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:663: "
{-# LINE 663 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 663 "src/Test/HMock/Predicates.hs" #-}
      (accept nonEmpty [1, 2, 3])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:665: "
{-# LINE 665 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 665 "src/Test/HMock/Predicates.hs" #-}
      (accept nonEmpty "")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:667: "
{-# LINE 667 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 667 "src/Test/HMock/Predicates.hs" #-}
      (accept nonEmpty "gas tank")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:675: "
{-# LINE 675 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 675 "src/Test/HMock/Predicates.hs" #-}
      (accept (sizeIs (lt 3)) ['a' .. 'f'])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:677: "
{-# LINE 677 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 677 "src/Test/HMock/Predicates.hs" #-}
      (accept (sizeIs (lt 3)) ['a' .. 'b'])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:694: "
{-# LINE 694 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 694 "src/Test/HMock/Predicates.hs" #-}
      (accept (elemsAre [lt 3, lt 4, lt 5]) [2, 3, 4])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:696: "
{-# LINE 696 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 696 "src/Test/HMock/Predicates.hs" #-}
      (accept (elemsAre [lt 3, lt 4, lt 5]) [2, 3, 4, 5])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:698: "
{-# LINE 698 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 698 "src/Test/HMock/Predicates.hs" #-}
      (accept (elemsAre [lt 3, lt 4, lt 5]) [2, 10, 4])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:714: "
{-# LINE 714 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 714 "src/Test/HMock/Predicates.hs" #-}
      (accept (unorderedElemsAre [eq 1, eq 2, eq 3]) [1, 2, 3])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:716: "
{-# LINE 716 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 716 "src/Test/HMock/Predicates.hs" #-}
      (accept (unorderedElemsAre [eq 1, eq 2, eq 3]) [2, 3, 1])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:718: "
{-# LINE 718 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 718 "src/Test/HMock/Predicates.hs" #-}
      (accept (unorderedElemsAre [eq 1, eq 2, eq 3]) [1, 2, 3, 4])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:720: "
{-# LINE 720 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 720 "src/Test/HMock/Predicates.hs" #-}
      (accept (unorderedElemsAre [eq 1, eq 2, eq 3]) [1, 3])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:739: "
{-# LINE 739 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 739 "src/Test/HMock/Predicates.hs" #-}
      (accept (each (gt 5)) [4, 5, 6])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:741: "
{-# LINE 741 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 741 "src/Test/HMock/Predicates.hs" #-}
      (accept (each (gt 5)) [6, 7, 8])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:743: "
{-# LINE 743 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 743 "src/Test/HMock/Predicates.hs" #-}
      (accept (each (gt 5)) [])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:757: "
{-# LINE 757 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 757 "src/Test/HMock/Predicates.hs" #-}
      (accept (contains (gt 5)) [3, 4, 5])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:759: "
{-# LINE 759 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 759 "src/Test/HMock/Predicates.hs" #-}
      (accept (contains (gt 5)) [4, 5, 6])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:761: "
{-# LINE 761 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 761 "src/Test/HMock/Predicates.hs" #-}
      (accept (contains (gt 5)) [])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:771: "
{-# LINE 771 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 771 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsAll [eq "foo", eq "bar"]) ["bar", "foo"])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:773: "
{-# LINE 773 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 773 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsAll [eq "foo", eq "bar"]) ["foo"])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:775: "
{-# LINE 775 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 775 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsAll [eq "foo", eq "bar"]) ["foo", "bar", "qux"])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:790: "
{-# LINE 790 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 790 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsOnly [eq "foo", eq "bar"]) ["foo", "foo"])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:792: "
{-# LINE 792 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 792 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsOnly [eq "foo", eq "bar"]) ["foo", "bar"])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:794: "
{-# LINE 794 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 794 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsOnly [eq "foo", eq "bar"]) ["foo", "qux"])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:808: "
{-# LINE 808 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 808 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsKey (eq "foo")) [("foo", 1), ("bar", 2)])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:810: "
{-# LINE 810 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 810 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsKey (eq "foo")) [("bar", 1), ("qux", 2)])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:825: "
{-# LINE 825 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 825 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsEntry (eq "foo") (gt 10)) [("foo", 12), ("bar", 5)])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:827: "
{-# LINE 827 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 827 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsEntry (eq "foo") (gt 10)) [("foo", 5), ("bar", 12)])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:829: "
{-# LINE 829 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 829 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsEntry (eq "foo") (gt 10)) [("bar", 12)])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:844: "
{-# LINE 844 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 844 "src/Test/HMock/Predicates.hs" #-}
      (accept (keysAre [eq "a", eq "b", eq "c"]) [("a", 1), ("b", 2), ("c", 3)])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:846: "
{-# LINE 846 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 846 "src/Test/HMock/Predicates.hs" #-}
      (accept (keysAre [eq "a", eq "b", eq "c"]) [("c", 1), ("b", 2), ("a", 3)])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:848: "
{-# LINE 848 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 848 "src/Test/HMock/Predicates.hs" #-}
      (accept (keysAre [eq "a", eq "b", eq "c"]) [("a", 1), ("c", 3)])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:850: "
{-# LINE 850 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 850 "src/Test/HMock/Predicates.hs" #-}
      (accept (keysAre [eq "a", eq "b"]) [("a", 1), ("b", 2), ("c", 3)])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:868: "
{-# LINE 868 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 868 "src/Test/HMock/Predicates.hs" #-}
      (accept (entriesAre [(eq 1, eq 2), (eq 3, eq 4)]) [(1, 2), (3, 4)])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:870: "
{-# LINE 870 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 870 "src/Test/HMock/Predicates.hs" #-}
      (accept (entriesAre [(eq 1, eq 2), (eq 3, eq 4)]) [(3, 4), (1, 2)])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:872: "
{-# LINE 872 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 872 "src/Test/HMock/Predicates.hs" #-}
      (accept (entriesAre [(eq 1, eq 2), (eq 3, eq 4)]) [(1, 4), (3, 2)])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:874: "
{-# LINE 874 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 874 "src/Test/HMock/Predicates.hs" #-}
      (accept (entriesAre [(eq 1, eq 2), (eq 3, eq 4)]) [(1, 2), (3, 4), (5, 6)])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:897: "
{-# LINE 897 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 897 "src/Test/HMock/Predicates.hs" #-}
      (accept (eq 1.0) (sum (replicate 100 0.01)))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:904: "
{-# LINE 904 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 904 "src/Test/HMock/Predicates.hs" #-}
      (accept (approxEq 1.0) (sum (replicate 100 0.01)))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:906: "
{-# LINE 906 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 906 "src/Test/HMock/Predicates.hs" #-}
      (accept (approxEq 1.0) (sum (replicate 100 0.009999)))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:921: "
{-# LINE 921 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 921 "src/Test/HMock/Predicates.hs" #-}
      (accept finite 1.0)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:923: "
{-# LINE 923 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 923 "src/Test/HMock/Predicates.hs" #-}
      (accept finite (0 / 0))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:925: "
{-# LINE 925 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 925 "src/Test/HMock/Predicates.hs" #-}
      (accept finite (1 / 0))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:938: "
{-# LINE 938 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 938 "src/Test/HMock/Predicates.hs" #-}
      (accept infinite 1.0)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:940: "
{-# LINE 940 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 940 "src/Test/HMock/Predicates.hs" #-}
      (accept infinite (0 / 0))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:942: "
{-# LINE 942 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 942 "src/Test/HMock/Predicates.hs" #-}
      (accept infinite (1 / 0))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:955: "
{-# LINE 955 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 955 "src/Test/HMock/Predicates.hs" #-}
      (accept nAn 1.0)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:957: "
{-# LINE 957 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 957 "src/Test/HMock/Predicates.hs" #-}
      (accept nAn (0 / 0))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:959: "
{-# LINE 959 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 959 "src/Test/HMock/Predicates.hs" #-}
      (accept nAn (1 / 0))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:974: "
{-# LINE 974 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 974 "src/Test/HMock/Predicates.hs" #-}
      (accept (is even) 3)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:976: "
{-# LINE 976 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 976 "src/Test/HMock/Predicates.hs" #-}
      (accept (is even) 4)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:991: "
{-# LINE 991 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 991 "src/Test/HMock/Predicates.hs" #-}
      (accept $(qIs [| even |]) 3)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:993: "
{-# LINE 993 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 993 "src/Test/HMock/Predicates.hs" #-}
      (accept $(qIs [| even |]) 4)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:996: "
{-# LINE 996 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 996 "src/Test/HMock/Predicates.hs" #-}
      (show $(qIs [| even |]))
  [ExpectedLine [LineChunk "\"even\""]]
 DocTest.printPrefix "Test.HMock.Predicates:1012: "
{-# LINE 1012 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1012 "src/Test/HMock/Predicates.hs" #-}
      (accept (with abs (gt 5)) (-6))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:1014: "
{-# LINE 1014 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1014 "src/Test/HMock/Predicates.hs" #-}
      (accept (with abs (gt 5)) (-5))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1016: "
{-# LINE 1016 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1016 "src/Test/HMock/Predicates.hs" #-}
      (accept (with reverse (eq "olleh")) "hello")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:1018: "
{-# LINE 1018 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1018 "src/Test/HMock/Predicates.hs" #-}
      (accept (with reverse (eq "olleh")) "goodbye")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1035: "
{-# LINE 1035 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1035 "src/Test/HMock/Predicates.hs" #-}
      (accept ($(qWith [| abs |]) (gt 5)) (-6))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:1037: "
{-# LINE 1037 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1037 "src/Test/HMock/Predicates.hs" #-}
      (accept ($(qWith [| abs |]) (gt 5)) (-5))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1039: "
{-# LINE 1039 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1039 "src/Test/HMock/Predicates.hs" #-}
      (accept ($(qWith [| reverse |]) (eq "olleh")) "hello")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:1041: "
{-# LINE 1041 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1041 "src/Test/HMock/Predicates.hs" #-}
      (accept ($(qWith [| reverse |]) (eq "olleh")) "goodbye")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1044: "
{-# LINE 1044 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1044 "src/Test/HMock/Predicates.hs" #-}
      (show ($(qWith [| abs |]) (gt 5)))
  [ExpectedLine [LineChunk "\"abs: > 5\""]]
 DocTest.printPrefix "Test.HMock.Predicates:1063: "
{-# LINE 1063 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1063 "src/Test/HMock/Predicates.hs" #-}
      (accept $(qMatch [p| Just (Left _) |]) Nothing)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1065: "
{-# LINE 1065 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1065 "src/Test/HMock/Predicates.hs" #-}
      (accept $(qMatch [p| Just (Left _) |]) (Just (Left 5)))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:1067: "
{-# LINE 1067 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1067 "src/Test/HMock/Predicates.hs" #-}
      (accept $(qMatch [p| Just (Left _) |]) (Just (Right 5)))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1070: "
{-# LINE 1070 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1070 "src/Test/HMock/Predicates.hs" #-}
      (show $(qMatch [p| Just (Left _) |]))
  [ExpectedLine [LineChunk "\"Just (Left _)\""]]
 DocTest.printPrefix "Test.HMock.Predicates:1088: "
{-# LINE 1088 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1088 "src/Test/HMock/Predicates.hs" #-}
      (accept (typed @String anything) "foo")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:1090: "
{-# LINE 1090 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1090 "src/Test/HMock/Predicates.hs" #-}
      (accept (typed @String (sizeIs (gt 5))) "foo")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1092: "
{-# LINE 1092 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1092 "src/Test/HMock/Predicates.hs" #-}
      (accept (typed @String anything) (42 :: Int))
  [ExpectedLine [LineChunk "False"]]
