-- Do not edit! Automatically created with doctest-extract from src/Test/HMock/Predicates.hs
{-# LINE 80 "src/Test/HMock/Predicates.hs" #-}

{-# OPTIONS_GHC -XTemplateHaskell #-}
{-# OPTIONS_GHC -XTypeApplications #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module DocTests.Test.HMock.Predicates where

import Test.HMock.Predicates
import Test.DocTest.Base
import qualified Test.DocTest.Driver as DocTest

{-# LINE 84 "src/Test/HMock/Predicates.hs" #-}

test :: DocTest.T ()
test = do
 DocTest.printPrefix "Test.HMock.Predicates:102: "
{-# LINE 102 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 102 "src/Test/HMock/Predicates.hs" #-}
      (accept anything "foo")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:104: "
{-# LINE 104 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 104 "src/Test/HMock/Predicates.hs" #-}
      (accept anything undefined)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:117: "
{-# LINE 117 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 117 "src/Test/HMock/Predicates.hs" #-}
      (accept (eq "foo") "foo")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:119: "
{-# LINE 119 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 119 "src/Test/HMock/Predicates.hs" #-}
      (accept (eq "foo") "bar")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:136: "
{-# LINE 136 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 136 "src/Test/HMock/Predicates.hs" #-}
      (accept (neq "foo") "foo")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:138: "
{-# LINE 138 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 138 "src/Test/HMock/Predicates.hs" #-}
      (accept (neq "foo") "bar")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:145: "
{-# LINE 145 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 145 "src/Test/HMock/Predicates.hs" #-}
      (accept (gt 5) 4)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:147: "
{-# LINE 147 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 147 "src/Test/HMock/Predicates.hs" #-}
      (accept (gt 5) 5)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:149: "
{-# LINE 149 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 149 "src/Test/HMock/Predicates.hs" #-}
      (accept (gt 5) 6)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:167: "
{-# LINE 167 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 167 "src/Test/HMock/Predicates.hs" #-}
      (accept (geq 5) 4)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:169: "
{-# LINE 169 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 169 "src/Test/HMock/Predicates.hs" #-}
      (accept (geq 5) 5)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:171: "
{-# LINE 171 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 171 "src/Test/HMock/Predicates.hs" #-}
      (accept (geq 5) 6)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:188: "
{-# LINE 188 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 188 "src/Test/HMock/Predicates.hs" #-}
      (accept (lt 5) 4)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:190: "
{-# LINE 190 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 190 "src/Test/HMock/Predicates.hs" #-}
      (accept (lt 5) 5)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:192: "
{-# LINE 192 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 192 "src/Test/HMock/Predicates.hs" #-}
      (accept (lt 5) 6)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:199: "
{-# LINE 199 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 199 "src/Test/HMock/Predicates.hs" #-}
      (accept (leq 5) 4)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:201: "
{-# LINE 201 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 201 "src/Test/HMock/Predicates.hs" #-}
      (accept (leq 5) 5)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:203: "
{-# LINE 203 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 203 "src/Test/HMock/Predicates.hs" #-}
      (accept (leq 5) 6)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:211: "
{-# LINE 211 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 211 "src/Test/HMock/Predicates.hs" #-}
      (accept (just (eq "value")) Nothing)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:213: "
{-# LINE 213 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 213 "src/Test/HMock/Predicates.hs" #-}
      (accept (just (eq "value")) (Just "value"))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:215: "
{-# LINE 215 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 215 "src/Test/HMock/Predicates.hs" #-}
      (accept (just (eq "value")) (Just "wrong value"))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:238: "
{-# LINE 238 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 238 "src/Test/HMock/Predicates.hs" #-}
      (accept (left (eq "value")) (Left "value"))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:240: "
{-# LINE 240 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 240 "src/Test/HMock/Predicates.hs" #-}
      (accept (left (eq "value")) (Right "value"))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:242: "
{-# LINE 242 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 242 "src/Test/HMock/Predicates.hs" #-}
      (accept (left (eq "value")) (Left "wrong value"))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:256: "
{-# LINE 256 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 256 "src/Test/HMock/Predicates.hs" #-}
      (accept (right (eq "value")) (Right "value"))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:258: "
{-# LINE 258 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 258 "src/Test/HMock/Predicates.hs" #-}
      (accept (right (eq "value")) (Right "wrong value"))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:260: "
{-# LINE 260 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 260 "src/Test/HMock/Predicates.hs" #-}
      (accept (right (eq "value")) (Left "value"))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:274: "
{-# LINE 274 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 274 "src/Test/HMock/Predicates.hs" #-}
      (accept (zipP (eq "foo") (eq "bar")) ("foo", "bar"))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:276: "
{-# LINE 276 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 276 "src/Test/HMock/Predicates.hs" #-}
      (accept (zipP (eq "foo") (eq "bar")) ("bar", "foo"))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:298: "
{-# LINE 298 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 298 "src/Test/HMock/Predicates.hs" #-}
      (accept (zip3P (eq "foo") (eq "bar") (eq "qux")) ("foo", "bar", "qux"))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:300: "
{-# LINE 300 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 300 "src/Test/HMock/Predicates.hs" #-}
      (accept (zip3P (eq "foo") (eq "bar") (eq "qux")) ("qux", "bar", "foo"))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:323: "
{-# LINE 323 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 323 "src/Test/HMock/Predicates.hs" #-}
      (accept (zip4P (eq 1) (eq 2) (eq 3) (eq 4)) (1, 2, 3, 4))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:325: "
{-# LINE 325 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 325 "src/Test/HMock/Predicates.hs" #-}
      (accept (zip4P (eq 1) (eq 2) (eq 3) (eq 4)) (4, 3, 2, 1))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:355: "
{-# LINE 355 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 355 "src/Test/HMock/Predicates.hs" #-}
      (accept (zip5P (eq 1) (eq 2) (eq 3) (eq 4) (eq 5)) (1, 2, 3, 4, 5))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:357: "
{-# LINE 357 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 357 "src/Test/HMock/Predicates.hs" #-}
      (accept (zip5P (eq 1) (eq 2) (eq 3) (eq 4) (eq 5)) (5, 4, 3, 2, 1))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:389: "
{-# LINE 389 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 389 "src/Test/HMock/Predicates.hs" #-}
      (accept (lt "foo" `andP` gt "bar") "eta")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:391: "
{-# LINE 391 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 391 "src/Test/HMock/Predicates.hs" #-}
      (accept (lt "foo" `andP` gt "bar") "quz")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:393: "
{-# LINE 393 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 393 "src/Test/HMock/Predicates.hs" #-}
      (accept (lt "foo" `andP` gt "bar") "alpha")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:412: "
{-# LINE 412 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 412 "src/Test/HMock/Predicates.hs" #-}
      (accept (lt "bar" `orP` gt "foo") "eta")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:414: "
{-# LINE 414 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 414 "src/Test/HMock/Predicates.hs" #-}
      (accept (lt "bar" `orP` gt "foo") "quz")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:416: "
{-# LINE 416 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 416 "src/Test/HMock/Predicates.hs" #-}
      (accept (lt "bar" `orP` gt "foo") "alpha")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:424: "
{-# LINE 424 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 424 "src/Test/HMock/Predicates.hs" #-}
      (accept (notP (eq "negative")) "positive")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:426: "
{-# LINE 426 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 426 "src/Test/HMock/Predicates.hs" #-}
      (accept (notP (eq "negative")) "negative")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:439: "
{-# LINE 439 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 439 "src/Test/HMock/Predicates.hs" #-}
      (accept (startsWith "fun") "fungible")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:441: "
{-# LINE 441 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 441 "src/Test/HMock/Predicates.hs" #-}
      (accept (startsWith "gib") "fungible")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:454: "
{-# LINE 454 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 454 "src/Test/HMock/Predicates.hs" #-}
      (accept (endsWith "ow") "crossbow")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:456: "
{-# LINE 456 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 456 "src/Test/HMock/Predicates.hs" #-}
      (accept (endsWith "ow") "trebuchet")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:470: "
{-# LINE 470 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 470 "src/Test/HMock/Predicates.hs" #-}
      (accept (hasSubstr "i") "team")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:472: "
{-# LINE 472 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 472 "src/Test/HMock/Predicates.hs" #-}
      (accept (hasSubstr "i") "partnership")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:486: "
{-# LINE 486 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 486 "src/Test/HMock/Predicates.hs" #-}
      (accept (hasSubsequence [1..5]) [1, 2, 3, 4, 5])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:488: "
{-# LINE 488 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 488 "src/Test/HMock/Predicates.hs" #-}
      (accept (hasSubsequence [1..5]) [0, 1, 0, 2, 0, 3, 0, 4, 0, 5, 0])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:490: "
{-# LINE 490 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 490 "src/Test/HMock/Predicates.hs" #-}
      (accept (hasSubsequence [1..5]) [2, 3, 5, 7, 11])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:504: "
{-# LINE 504 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 504 "src/Test/HMock/Predicates.hs" #-}
      (accept (caseInsensitive startsWith "foo") "FOOTBALL!")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:506: "
{-# LINE 506 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 506 "src/Test/HMock/Predicates.hs" #-}
      (accept (caseInsensitive endsWith "ball") "soccer")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:508: "
{-# LINE 508 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 508 "src/Test/HMock/Predicates.hs" #-}
      (accept (caseInsensitive eq "time") "TIME")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:510: "
{-# LINE 510 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 510 "src/Test/HMock/Predicates.hs" #-}
      (accept (caseInsensitive gt "NOTHING") "everything")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:537: "
{-# LINE 537 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 537 "src/Test/HMock/Predicates.hs" #-}
      (accept (matchesRegex "x{2,5}y?") "xxxy")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:539: "
{-# LINE 539 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 539 "src/Test/HMock/Predicates.hs" #-}
      (accept (matchesRegex "x{2,5}y?") "xyy")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:541: "
{-# LINE 541 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 541 "src/Test/HMock/Predicates.hs" #-}
      (accept (matchesRegex "x{2,5}y?") "wxxxyz")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:572: "
{-# LINE 572 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 572 "src/Test/HMock/Predicates.hs" #-}
      (accept (matchesCaseInsensitiveRegex "x{2,5}y?") "XXXY")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:574: "
{-# LINE 574 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 574 "src/Test/HMock/Predicates.hs" #-}
      (accept (matchesCaseInsensitiveRegex "x{2,5}y?") "XYY")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:576: "
{-# LINE 576 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 576 "src/Test/HMock/Predicates.hs" #-}
      (accept (matchesCaseInsensitiveRegex "x{2,5}y?") "WXXXYZ")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:613: "
{-# LINE 613 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 613 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsRegex "x{2,5}y?") "xxxy")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:615: "
{-# LINE 615 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 615 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsRegex "x{2,5}y?") "xyy")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:617: "
{-# LINE 617 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 617 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsRegex "x{2,5}y?") "wxxxyz")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:645: "
{-# LINE 645 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 645 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsCaseInsensitiveRegex "x{2,5}y?") "XXXY")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:647: "
{-# LINE 647 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 647 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsCaseInsensitiveRegex "x{2,5}y?") "XYY")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:649: "
{-# LINE 649 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 649 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsCaseInsensitiveRegex "x{2,5}y?") "WXXXYZ")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:677: "
{-# LINE 677 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 677 "src/Test/HMock/Predicates.hs" #-}
      (accept isEmpty ([] :: [Int]))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:679: "
{-# LINE 679 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 679 "src/Test/HMock/Predicates.hs" #-}
      (accept isEmpty [1, 2, 3])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:681: "
{-# LINE 681 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 681 "src/Test/HMock/Predicates.hs" #-}
      (accept isEmpty "")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:683: "
{-# LINE 683 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 683 "src/Test/HMock/Predicates.hs" #-}
      (accept isEmpty "gas tank")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:700: "
{-# LINE 700 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 700 "src/Test/HMock/Predicates.hs" #-}
      (accept nonEmpty ([] :: [Int]))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:702: "
{-# LINE 702 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 702 "src/Test/HMock/Predicates.hs" #-}
      (accept nonEmpty [1, 2, 3])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:704: "
{-# LINE 704 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 704 "src/Test/HMock/Predicates.hs" #-}
      (accept nonEmpty "")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:706: "
{-# LINE 706 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 706 "src/Test/HMock/Predicates.hs" #-}
      (accept nonEmpty "gas tank")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:714: "
{-# LINE 714 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 714 "src/Test/HMock/Predicates.hs" #-}
      (accept (sizeIs (lt 3)) ['a' .. 'f'])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:716: "
{-# LINE 716 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 716 "src/Test/HMock/Predicates.hs" #-}
      (accept (sizeIs (lt 3)) ['a' .. 'b'])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:733: "
{-# LINE 733 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 733 "src/Test/HMock/Predicates.hs" #-}
      (accept (elemsAre [lt 3, lt 4, lt 5]) [2, 3, 4])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:735: "
{-# LINE 735 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 735 "src/Test/HMock/Predicates.hs" #-}
      (accept (elemsAre [lt 3, lt 4, lt 5]) [2, 3, 4, 5])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:737: "
{-# LINE 737 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 737 "src/Test/HMock/Predicates.hs" #-}
      (accept (elemsAre [lt 3, lt 4, lt 5]) [2, 10, 4])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:753: "
{-# LINE 753 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 753 "src/Test/HMock/Predicates.hs" #-}
      (accept (unorderedElemsAre [eq 1, eq 2, eq 3]) [1, 2, 3])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:755: "
{-# LINE 755 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 755 "src/Test/HMock/Predicates.hs" #-}
      (accept (unorderedElemsAre [eq 1, eq 2, eq 3]) [2, 3, 1])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:757: "
{-# LINE 757 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 757 "src/Test/HMock/Predicates.hs" #-}
      (accept (unorderedElemsAre [eq 1, eq 2, eq 3]) [1, 2, 3, 4])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:759: "
{-# LINE 759 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 759 "src/Test/HMock/Predicates.hs" #-}
      (accept (unorderedElemsAre [eq 1, eq 2, eq 3]) [1, 3])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:778: "
{-# LINE 778 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 778 "src/Test/HMock/Predicates.hs" #-}
      (accept (each (gt 5)) [4, 5, 6])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:780: "
{-# LINE 780 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 780 "src/Test/HMock/Predicates.hs" #-}
      (accept (each (gt 5)) [6, 7, 8])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:782: "
{-# LINE 782 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 782 "src/Test/HMock/Predicates.hs" #-}
      (accept (each (gt 5)) [])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:796: "
{-# LINE 796 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 796 "src/Test/HMock/Predicates.hs" #-}
      (accept (contains (gt 5)) [3, 4, 5])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:798: "
{-# LINE 798 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 798 "src/Test/HMock/Predicates.hs" #-}
      (accept (contains (gt 5)) [4, 5, 6])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:800: "
{-# LINE 800 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 800 "src/Test/HMock/Predicates.hs" #-}
      (accept (contains (gt 5)) [])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:810: "
{-# LINE 810 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 810 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsAll [eq "foo", eq "bar"]) ["bar", "foo"])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:812: "
{-# LINE 812 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 812 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsAll [eq "foo", eq "bar"]) ["foo"])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:814: "
{-# LINE 814 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 814 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsAll [eq "foo", eq "bar"]) ["foo", "bar", "qux"])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:829: "
{-# LINE 829 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 829 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsOnly [eq "foo", eq "bar"]) ["foo", "foo"])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:831: "
{-# LINE 831 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 831 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsOnly [eq "foo", eq "bar"]) ["foo", "bar"])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:833: "
{-# LINE 833 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 833 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsOnly [eq "foo", eq "bar"]) ["foo", "qux"])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:847: "
{-# LINE 847 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 847 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsKey (eq "foo")) [("foo", 1), ("bar", 2)])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:849: "
{-# LINE 849 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 849 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsKey (eq "foo")) [("bar", 1), ("qux", 2)])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:864: "
{-# LINE 864 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 864 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsEntry (eq "foo") (gt 10)) [("foo", 12), ("bar", 5)])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:866: "
{-# LINE 866 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 866 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsEntry (eq "foo") (gt 10)) [("foo", 5), ("bar", 12)])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:868: "
{-# LINE 868 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 868 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsEntry (eq "foo") (gt 10)) [("bar", 12)])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:883: "
{-# LINE 883 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 883 "src/Test/HMock/Predicates.hs" #-}
      (accept (keysAre [eq "a", eq "b", eq "c"]) [("a", 1), ("b", 2), ("c", 3)])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:885: "
{-# LINE 885 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 885 "src/Test/HMock/Predicates.hs" #-}
      (accept (keysAre [eq "a", eq "b", eq "c"]) [("c", 1), ("b", 2), ("a", 3)])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:887: "
{-# LINE 887 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 887 "src/Test/HMock/Predicates.hs" #-}
      (accept (keysAre [eq "a", eq "b", eq "c"]) [("a", 1), ("c", 3)])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:889: "
{-# LINE 889 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 889 "src/Test/HMock/Predicates.hs" #-}
      (accept (keysAre [eq "a", eq "b"]) [("a", 1), ("b", 2), ("c", 3)])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:907: "
{-# LINE 907 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 907 "src/Test/HMock/Predicates.hs" #-}
      (accept (entriesAre [(eq 1, eq 2), (eq 3, eq 4)]) [(1, 2), (3, 4)])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:909: "
{-# LINE 909 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 909 "src/Test/HMock/Predicates.hs" #-}
      (accept (entriesAre [(eq 1, eq 2), (eq 3, eq 4)]) [(3, 4), (1, 2)])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:911: "
{-# LINE 911 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 911 "src/Test/HMock/Predicates.hs" #-}
      (accept (entriesAre [(eq 1, eq 2), (eq 3, eq 4)]) [(1, 4), (3, 2)])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:913: "
{-# LINE 913 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 913 "src/Test/HMock/Predicates.hs" #-}
      (accept (entriesAre [(eq 1, eq 2), (eq 3, eq 4)]) [(1, 2), (3, 4), (5, 6)])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:936: "
{-# LINE 936 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 936 "src/Test/HMock/Predicates.hs" #-}
      (accept (eq 1.0) (sum (replicate 100 0.01)))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:943: "
{-# LINE 943 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 943 "src/Test/HMock/Predicates.hs" #-}
      (accept (approxEq 1.0) (sum (replicate 100 0.01)))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:945: "
{-# LINE 945 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 945 "src/Test/HMock/Predicates.hs" #-}
      (accept (approxEq 1.0) (sum (replicate 100 0.009999)))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:960: "
{-# LINE 960 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 960 "src/Test/HMock/Predicates.hs" #-}
      (accept finite 1.0)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:962: "
{-# LINE 962 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 962 "src/Test/HMock/Predicates.hs" #-}
      (accept finite (0 / 0))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:964: "
{-# LINE 964 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 964 "src/Test/HMock/Predicates.hs" #-}
      (accept finite (1 / 0))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:977: "
{-# LINE 977 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 977 "src/Test/HMock/Predicates.hs" #-}
      (accept infinite 1.0)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:979: "
{-# LINE 979 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 979 "src/Test/HMock/Predicates.hs" #-}
      (accept infinite (0 / 0))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:981: "
{-# LINE 981 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 981 "src/Test/HMock/Predicates.hs" #-}
      (accept infinite (1 / 0))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:994: "
{-# LINE 994 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 994 "src/Test/HMock/Predicates.hs" #-}
      (accept nAn 1.0)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:996: "
{-# LINE 996 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 996 "src/Test/HMock/Predicates.hs" #-}
      (accept nAn (0 / 0))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:998: "
{-# LINE 998 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 998 "src/Test/HMock/Predicates.hs" #-}
      (accept nAn (1 / 0))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1013: "
{-# LINE 1013 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1013 "src/Test/HMock/Predicates.hs" #-}
      (accept (is even) 3)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1015: "
{-# LINE 1015 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1015 "src/Test/HMock/Predicates.hs" #-}
      (accept (is even) 4)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:1030: "
{-# LINE 1030 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1030 "src/Test/HMock/Predicates.hs" #-}
      (accept $(qIs [| even |]) 3)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1032: "
{-# LINE 1032 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1032 "src/Test/HMock/Predicates.hs" #-}
      (accept $(qIs [| even |]) 4)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:1035: "
{-# LINE 1035 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1035 "src/Test/HMock/Predicates.hs" #-}
      (show $(qIs [| even |]))
  [ExpectedLine [LineChunk "\"even\""]]
 DocTest.printPrefix "Test.HMock.Predicates:1051: "
{-# LINE 1051 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1051 "src/Test/HMock/Predicates.hs" #-}
      (accept (with abs (gt 5)) (-6))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:1053: "
{-# LINE 1053 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1053 "src/Test/HMock/Predicates.hs" #-}
      (accept (with abs (gt 5)) (-5))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1055: "
{-# LINE 1055 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1055 "src/Test/HMock/Predicates.hs" #-}
      (accept (with reverse (eq "olleh")) "hello")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:1057: "
{-# LINE 1057 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1057 "src/Test/HMock/Predicates.hs" #-}
      (accept (with reverse (eq "olleh")) "goodbye")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1074: "
{-# LINE 1074 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1074 "src/Test/HMock/Predicates.hs" #-}
      (accept ($(qWith [| abs |]) (gt 5)) (-6))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:1076: "
{-# LINE 1076 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1076 "src/Test/HMock/Predicates.hs" #-}
      (accept ($(qWith [| abs |]) (gt 5)) (-5))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1078: "
{-# LINE 1078 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1078 "src/Test/HMock/Predicates.hs" #-}
      (accept ($(qWith [| reverse |]) (eq "olleh")) "hello")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:1080: "
{-# LINE 1080 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1080 "src/Test/HMock/Predicates.hs" #-}
      (accept ($(qWith [| reverse |]) (eq "olleh")) "goodbye")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1083: "
{-# LINE 1083 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1083 "src/Test/HMock/Predicates.hs" #-}
      (show ($(qWith [| abs |]) (gt 5)))
  [ExpectedLine [LineChunk "\"abs: > 5\""]]
 DocTest.printPrefix "Test.HMock.Predicates:1102: "
{-# LINE 1102 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1102 "src/Test/HMock/Predicates.hs" #-}
      (accept $(qMatch [p| Just (Left _) |]) Nothing)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1104: "
{-# LINE 1104 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1104 "src/Test/HMock/Predicates.hs" #-}
      (accept $(qMatch [p| Just (Left _) |]) (Just (Left 5)))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:1106: "
{-# LINE 1106 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1106 "src/Test/HMock/Predicates.hs" #-}
      (accept $(qMatch [p| Just (Left _) |]) (Just (Right 5)))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1109: "
{-# LINE 1109 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1109 "src/Test/HMock/Predicates.hs" #-}
      (show $(qMatch [p| Just (Left _) |]))
  [ExpectedLine [LineChunk "\"Just (Left _)\""]]
 DocTest.printPrefix "Test.HMock.Predicates:1127: "
{-# LINE 1127 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1127 "src/Test/HMock/Predicates.hs" #-}
      (accept (typed @String anything) "foo")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:1129: "
{-# LINE 1129 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1129 "src/Test/HMock/Predicates.hs" #-}
      (accept (typed @String (sizeIs (gt 5))) "foo")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1131: "
{-# LINE 1131 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1131 "src/Test/HMock/Predicates.hs" #-}
      (accept (typed @String anything) (42 :: Int))
  [ExpectedLine [LineChunk "False"]]
