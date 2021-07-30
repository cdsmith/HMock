-- Do not edit! Automatically created with doctest-extract from src/Test/HMock/Predicates.hs
{-# LINE 84 "src/Test/HMock/Predicates.hs" #-}

{-# OPTIONS_GHC -XTemplateHaskell #-}
{-# OPTIONS_GHC -XTypeApplications #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module DocTests.Test.HMock.Predicates where

import Test.HMock.Predicates
import Test.DocTest.Base
import qualified Test.DocTest.Driver as DocTest

{-# LINE 88 "src/Test/HMock/Predicates.hs" #-}

test :: DocTest.T ()
test = do
 DocTest.printPrefix "Test.HMock.Predicates:115: "
{-# LINE 115 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 115 "src/Test/HMock/Predicates.hs" #-}
      (accept anything "foo")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:117: "
{-# LINE 117 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 117 "src/Test/HMock/Predicates.hs" #-}
      (accept anything undefined)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:130: "
{-# LINE 130 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 130 "src/Test/HMock/Predicates.hs" #-}
      (accept (eq "foo") "foo")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:132: "
{-# LINE 132 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 132 "src/Test/HMock/Predicates.hs" #-}
      (accept (eq "foo") "bar")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:148: "
{-# LINE 148 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 148 "src/Test/HMock/Predicates.hs" #-}
      (accept (neq "foo") "foo")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:150: "
{-# LINE 150 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 150 "src/Test/HMock/Predicates.hs" #-}
      (accept (neq "foo") "bar")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:157: "
{-# LINE 157 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 157 "src/Test/HMock/Predicates.hs" #-}
      (accept (gt 5) 4)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:159: "
{-# LINE 159 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 159 "src/Test/HMock/Predicates.hs" #-}
      (accept (gt 5) 5)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:161: "
{-# LINE 161 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 161 "src/Test/HMock/Predicates.hs" #-}
      (accept (gt 5) 6)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:175: "
{-# LINE 175 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 175 "src/Test/HMock/Predicates.hs" #-}
      (accept (geq 5) 4)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:177: "
{-# LINE 177 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 177 "src/Test/HMock/Predicates.hs" #-}
      (accept (geq 5) 5)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:179: "
{-# LINE 179 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 179 "src/Test/HMock/Predicates.hs" #-}
      (accept (geq 5) 6)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:192: "
{-# LINE 192 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 192 "src/Test/HMock/Predicates.hs" #-}
      (accept (lt 5) 4)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:194: "
{-# LINE 194 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 194 "src/Test/HMock/Predicates.hs" #-}
      (accept (lt 5) 5)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:196: "
{-# LINE 196 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 196 "src/Test/HMock/Predicates.hs" #-}
      (accept (lt 5) 6)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:203: "
{-# LINE 203 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 203 "src/Test/HMock/Predicates.hs" #-}
      (accept (leq 5) 4)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:205: "
{-# LINE 205 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 205 "src/Test/HMock/Predicates.hs" #-}
      (accept (leq 5) 5)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:207: "
{-# LINE 207 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 207 "src/Test/HMock/Predicates.hs" #-}
      (accept (leq 5) 6)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:215: "
{-# LINE 215 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 215 "src/Test/HMock/Predicates.hs" #-}
      (accept (just (eq "value")) Nothing)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:217: "
{-# LINE 217 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 217 "src/Test/HMock/Predicates.hs" #-}
      (accept (just (eq "value")) (Just "value"))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:219: "
{-# LINE 219 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 219 "src/Test/HMock/Predicates.hs" #-}
      (accept (just (eq "value")) (Just "wrong value"))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:233: "
{-# LINE 233 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 233 "src/Test/HMock/Predicates.hs" #-}
      (accept nothing Nothing)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:236: "
{-# LINE 236 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 236 "src/Test/HMock/Predicates.hs" #-}
      (accept nothing (Just "something"))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:250: "
{-# LINE 250 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 250 "src/Test/HMock/Predicates.hs" #-}
      (accept (left (eq "value")) (Left "value"))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:252: "
{-# LINE 252 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 252 "src/Test/HMock/Predicates.hs" #-}
      (accept (left (eq "value")) (Right "value"))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:254: "
{-# LINE 254 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 254 "src/Test/HMock/Predicates.hs" #-}
      (accept (left (eq "value")) (Left "wrong value"))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:268: "
{-# LINE 268 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 268 "src/Test/HMock/Predicates.hs" #-}
      (accept (right (eq "value")) (Right "value"))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:270: "
{-# LINE 270 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 270 "src/Test/HMock/Predicates.hs" #-}
      (accept (right (eq "value")) (Right "wrong value"))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:272: "
{-# LINE 272 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 272 "src/Test/HMock/Predicates.hs" #-}
      (accept (right (eq "value")) (Left "value"))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:286: "
{-# LINE 286 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 286 "src/Test/HMock/Predicates.hs" #-}
      (accept (zipP (eq "foo") (eq "bar")) ("foo", "bar"))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:288: "
{-# LINE 288 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 288 "src/Test/HMock/Predicates.hs" #-}
      (accept (zipP (eq "foo") (eq "bar")) ("bar", "foo"))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:312: "
{-# LINE 312 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 312 "src/Test/HMock/Predicates.hs" #-}
      (accept (zip3P (eq "foo") (eq "bar") (eq "qux")) ("foo", "bar", "qux"))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:314: "
{-# LINE 314 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 314 "src/Test/HMock/Predicates.hs" #-}
      (accept (zip3P (eq "foo") (eq "bar") (eq "qux")) ("qux", "bar", "foo"))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:339: "
{-# LINE 339 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 339 "src/Test/HMock/Predicates.hs" #-}
      (accept (zip4P (eq 1) (eq 2) (eq 3) (eq 4)) (1, 2, 3, 4))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:341: "
{-# LINE 341 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 341 "src/Test/HMock/Predicates.hs" #-}
      (accept (zip4P (eq 1) (eq 2) (eq 3) (eq 4)) (4, 3, 2, 1))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:372: "
{-# LINE 372 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 372 "src/Test/HMock/Predicates.hs" #-}
      (accept (zip5P (eq 1) (eq 2) (eq 3) (eq 4) (eq 5)) (1, 2, 3, 4, 5))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:374: "
{-# LINE 374 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 374 "src/Test/HMock/Predicates.hs" #-}
      (accept (zip5P (eq 1) (eq 2) (eq 3) (eq 4) (eq 5)) (5, 4, 3, 2, 1))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:406: "
{-# LINE 406 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 406 "src/Test/HMock/Predicates.hs" #-}
      (accept (lt "foo" `andP` gt "bar") "eta")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:408: "
{-# LINE 408 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 408 "src/Test/HMock/Predicates.hs" #-}
      (accept (lt "foo" `andP` gt "bar") "quz")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:410: "
{-# LINE 410 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 410 "src/Test/HMock/Predicates.hs" #-}
      (accept (lt "foo" `andP` gt "bar") "alpha")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:427: "
{-# LINE 427 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 427 "src/Test/HMock/Predicates.hs" #-}
      (accept (lt "bar" `orP` gt "foo") "eta")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:429: "
{-# LINE 429 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 429 "src/Test/HMock/Predicates.hs" #-}
      (accept (lt "bar" `orP` gt "foo") "quz")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:431: "
{-# LINE 431 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 431 "src/Test/HMock/Predicates.hs" #-}
      (accept (lt "bar" `orP` gt "foo") "alpha")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:439: "
{-# LINE 439 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 439 "src/Test/HMock/Predicates.hs" #-}
      (accept (notP (eq "negative")) "positive")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:441: "
{-# LINE 441 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 441 "src/Test/HMock/Predicates.hs" #-}
      (accept (notP (eq "negative")) "negative")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:454: "
{-# LINE 454 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 454 "src/Test/HMock/Predicates.hs" #-}
      (accept (startsWith "fun") "fungible")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:456: "
{-# LINE 456 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 456 "src/Test/HMock/Predicates.hs" #-}
      (accept (startsWith "gib") "fungible")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:469: "
{-# LINE 469 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 469 "src/Test/HMock/Predicates.hs" #-}
      (accept (endsWith "ow") "crossbow")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:471: "
{-# LINE 471 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 471 "src/Test/HMock/Predicates.hs" #-}
      (accept (endsWith "ow") "trebuchet")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:485: "
{-# LINE 485 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 485 "src/Test/HMock/Predicates.hs" #-}
      (accept (hasSubstr "i") "team")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:487: "
{-# LINE 487 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 487 "src/Test/HMock/Predicates.hs" #-}
      (accept (hasSubstr "i") "partnership")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:501: "
{-# LINE 501 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 501 "src/Test/HMock/Predicates.hs" #-}
      (accept (hasSubsequence [1..5]) [1, 2, 3, 4, 5])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:503: "
{-# LINE 503 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 503 "src/Test/HMock/Predicates.hs" #-}
      (accept (hasSubsequence [1..5]) [0, 1, 0, 2, 0, 3, 0, 4, 0, 5, 0])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:505: "
{-# LINE 505 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 505 "src/Test/HMock/Predicates.hs" #-}
      (accept (hasSubsequence [1..5]) [2, 3, 5, 7, 11])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:519: "
{-# LINE 519 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 519 "src/Test/HMock/Predicates.hs" #-}
      (accept (caseInsensitive startsWith "foo") "FOOTBALL!")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:521: "
{-# LINE 521 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 521 "src/Test/HMock/Predicates.hs" #-}
      (accept (caseInsensitive endsWith "ball") "soccer")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:523: "
{-# LINE 523 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 523 "src/Test/HMock/Predicates.hs" #-}
      (accept (caseInsensitive eq "time") "TIME")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:525: "
{-# LINE 525 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 525 "src/Test/HMock/Predicates.hs" #-}
      (accept (caseInsensitive gt "NOTHING") "everything")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:552: "
{-# LINE 552 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 552 "src/Test/HMock/Predicates.hs" #-}
      (accept (matchesRegex "x{2,5}y?") "xxxy")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:554: "
{-# LINE 554 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 554 "src/Test/HMock/Predicates.hs" #-}
      (accept (matchesRegex "x{2,5}y?") "xyy")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:556: "
{-# LINE 556 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 556 "src/Test/HMock/Predicates.hs" #-}
      (accept (matchesRegex "x{2,5}y?") "wxxxyz")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:586: "
{-# LINE 586 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 586 "src/Test/HMock/Predicates.hs" #-}
      (accept (matchesCaseInsensitiveRegex "x{2,5}y?") "XXXY")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:588: "
{-# LINE 588 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 588 "src/Test/HMock/Predicates.hs" #-}
      (accept (matchesCaseInsensitiveRegex "x{2,5}y?") "XYY")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:590: "
{-# LINE 590 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 590 "src/Test/HMock/Predicates.hs" #-}
      (accept (matchesCaseInsensitiveRegex "x{2,5}y?") "WXXXYZ")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:626: "
{-# LINE 626 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 626 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsRegex "x{2,5}y?") "xxxy")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:628: "
{-# LINE 628 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 628 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsRegex "x{2,5}y?") "xyy")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:630: "
{-# LINE 630 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 630 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsRegex "x{2,5}y?") "wxxxyz")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:654: "
{-# LINE 654 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 654 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsCaseInsensitiveRegex "x{2,5}y?") "XXXY")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:656: "
{-# LINE 656 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 656 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsCaseInsensitiveRegex "x{2,5}y?") "XYY")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:658: "
{-# LINE 658 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 658 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsCaseInsensitiveRegex "x{2,5}y?") "WXXXYZ")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:682: "
{-# LINE 682 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 682 "src/Test/HMock/Predicates.hs" #-}
      (accept isEmpty ([] :: [Int]))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:684: "
{-# LINE 684 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 684 "src/Test/HMock/Predicates.hs" #-}
      (accept isEmpty [1, 2, 3])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:686: "
{-# LINE 686 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 686 "src/Test/HMock/Predicates.hs" #-}
      (accept isEmpty "")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:688: "
{-# LINE 688 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 688 "src/Test/HMock/Predicates.hs" #-}
      (accept isEmpty "gas tank")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:701: "
{-# LINE 701 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 701 "src/Test/HMock/Predicates.hs" #-}
      (accept nonEmpty ([] :: [Int]))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:703: "
{-# LINE 703 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 703 "src/Test/HMock/Predicates.hs" #-}
      (accept nonEmpty [1, 2, 3])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:705: "
{-# LINE 705 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 705 "src/Test/HMock/Predicates.hs" #-}
      (accept nonEmpty "")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:707: "
{-# LINE 707 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 707 "src/Test/HMock/Predicates.hs" #-}
      (accept nonEmpty "gas tank")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:715: "
{-# LINE 715 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 715 "src/Test/HMock/Predicates.hs" #-}
      (accept (sizeIs (lt 3)) ['a' .. 'f'])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:717: "
{-# LINE 717 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 717 "src/Test/HMock/Predicates.hs" #-}
      (accept (sizeIs (lt 3)) ['a' .. 'b'])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:738: "
{-# LINE 738 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 738 "src/Test/HMock/Predicates.hs" #-}
      (accept (elemsAre [lt 3, lt 4, lt 5]) [2, 3, 4])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:740: "
{-# LINE 740 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 740 "src/Test/HMock/Predicates.hs" #-}
      (accept (elemsAre [lt 3, lt 4, lt 5]) [2, 3, 4, 5])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:742: "
{-# LINE 742 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 742 "src/Test/HMock/Predicates.hs" #-}
      (accept (elemsAre [lt 3, lt 4, lt 5]) [2, 10, 4])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:774: "
{-# LINE 774 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 774 "src/Test/HMock/Predicates.hs" #-}
      (accept (unorderedElemsAre [eq 1, eq 2, eq 3]) [1, 2, 3])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:776: "
{-# LINE 776 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 776 "src/Test/HMock/Predicates.hs" #-}
      (accept (unorderedElemsAre [eq 1, eq 2, eq 3]) [2, 3, 1])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:778: "
{-# LINE 778 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 778 "src/Test/HMock/Predicates.hs" #-}
      (accept (unorderedElemsAre [eq 1, eq 2, eq 3]) [1, 2, 3, 4])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:780: "
{-# LINE 780 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 780 "src/Test/HMock/Predicates.hs" #-}
      (accept (unorderedElemsAre [eq 1, eq 2, eq 3]) [1, 3])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:808: "
{-# LINE 808 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 808 "src/Test/HMock/Predicates.hs" #-}
      (accept (each (gt 5)) [4, 5, 6])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:810: "
{-# LINE 810 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 810 "src/Test/HMock/Predicates.hs" #-}
      (accept (each (gt 5)) [6, 7, 8])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:812: "
{-# LINE 812 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 812 "src/Test/HMock/Predicates.hs" #-}
      (accept (each (gt 5)) [])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:837: "
{-# LINE 837 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 837 "src/Test/HMock/Predicates.hs" #-}
      (accept (contains (gt 5)) [3, 4, 5])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:839: "
{-# LINE 839 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 839 "src/Test/HMock/Predicates.hs" #-}
      (accept (contains (gt 5)) [4, 5, 6])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:841: "
{-# LINE 841 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 841 "src/Test/HMock/Predicates.hs" #-}
      (accept (contains (gt 5)) [])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:851: "
{-# LINE 851 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 851 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsAll [eq "foo", eq "bar"]) ["bar", "foo"])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:853: "
{-# LINE 853 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 853 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsAll [eq "foo", eq "bar"]) ["foo"])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:855: "
{-# LINE 855 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 855 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsAll [eq "foo", eq "bar"]) ["foo", "bar", "qux"])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:873: "
{-# LINE 873 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 873 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsOnly [eq "foo", eq "bar"]) ["foo", "foo"])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:875: "
{-# LINE 875 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 875 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsOnly [eq "foo", eq "bar"]) ["foo", "bar"])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:877: "
{-# LINE 877 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 877 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsOnly [eq "foo", eq "bar"]) ["foo", "qux"])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:896: "
{-# LINE 896 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 896 "src/Test/HMock/Predicates.hs" #-}
      (accept (keys (each (eq "foo"))) [("foo", 5)])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:899: "
{-# LINE 899 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 899 "src/Test/HMock/Predicates.hs" #-}
      (accept (keys (each (eq "foo"))) [("foo", 5), ("bar", 6)])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:915: "
{-# LINE 915 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 915 "src/Test/HMock/Predicates.hs" #-}
      (accept (values (each (eq 5))) [("foo", 5), ("bar", 5)])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:918: "
{-# LINE 918 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 918 "src/Test/HMock/Predicates.hs" #-}
      (accept (values (each (eq 5))) [("foo", 5), ("bar", 6)])
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
      (accept positive 1)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:963: "
{-# LINE 963 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 963 "src/Test/HMock/Predicates.hs" #-}
      (accept positive 0)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:966: "
{-# LINE 966 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 966 "src/Test/HMock/Predicates.hs" #-}
      (accept positive (-1))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:984: "
{-# LINE 984 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 984 "src/Test/HMock/Predicates.hs" #-}
      (accept negative 1)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:987: "
{-# LINE 987 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 987 "src/Test/HMock/Predicates.hs" #-}
      (accept negative 0)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:990: "
{-# LINE 990 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 990 "src/Test/HMock/Predicates.hs" #-}
      (accept negative (-1))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:1009: "
{-# LINE 1009 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1009 "src/Test/HMock/Predicates.hs" #-}
      (accept nonPositive 1)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1012: "
{-# LINE 1012 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1012 "src/Test/HMock/Predicates.hs" #-}
      (accept nonPositive 0)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:1015: "
{-# LINE 1015 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1015 "src/Test/HMock/Predicates.hs" #-}
      (accept nonPositive (-1))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:1023: "
{-# LINE 1023 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1023 "src/Test/HMock/Predicates.hs" #-}
      (accept nonNegative 1)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:1026: "
{-# LINE 1026 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1026 "src/Test/HMock/Predicates.hs" #-}
      (accept nonNegative 0)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:1029: "
{-# LINE 1029 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1029 "src/Test/HMock/Predicates.hs" #-}
      (accept nonNegative (-1))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1036: "
{-# LINE 1036 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1036 "src/Test/HMock/Predicates.hs" #-}
      (accept finite 1.0)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:1038: "
{-# LINE 1038 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1038 "src/Test/HMock/Predicates.hs" #-}
      (accept finite (0 / 0))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1040: "
{-# LINE 1040 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1040 "src/Test/HMock/Predicates.hs" #-}
      (accept finite (1 / 0))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1058: "
{-# LINE 1058 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1058 "src/Test/HMock/Predicates.hs" #-}
      (accept infinite 1.0)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1060: "
{-# LINE 1060 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1060 "src/Test/HMock/Predicates.hs" #-}
      (accept infinite (0 / 0))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1062: "
{-# LINE 1062 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1062 "src/Test/HMock/Predicates.hs" #-}
      (accept infinite (1 / 0))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:1078: "
{-# LINE 1078 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1078 "src/Test/HMock/Predicates.hs" #-}
      (accept nAn 1.0)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1080: "
{-# LINE 1080 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1080 "src/Test/HMock/Predicates.hs" #-}
      (accept nAn (0 / 0))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:1082: "
{-# LINE 1082 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1082 "src/Test/HMock/Predicates.hs" #-}
      (accept nAn (1 / 0))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1100: "
{-# LINE 1100 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1100 "src/Test/HMock/Predicates.hs" #-}
      (accept (is even) 3)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1102: "
{-# LINE 1102 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1102 "src/Test/HMock/Predicates.hs" #-}
      (accept (is even) 4)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:1120: "
{-# LINE 1120 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1120 "src/Test/HMock/Predicates.hs" #-}
      (accept $(qIs [| even |]) 3)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1122: "
{-# LINE 1122 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1122 "src/Test/HMock/Predicates.hs" #-}
      (accept $(qIs [| even |]) 4)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:1125: "
{-# LINE 1125 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1125 "src/Test/HMock/Predicates.hs" #-}
      (show $(qIs [| even |]))
  [ExpectedLine [LineChunk "\"even\""]]
 DocTest.printPrefix "Test.HMock.Predicates:1143: "
{-# LINE 1143 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1143 "src/Test/HMock/Predicates.hs" #-}
      (accept (with abs (gt 5)) (-6))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:1145: "
{-# LINE 1145 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1145 "src/Test/HMock/Predicates.hs" #-}
      (accept (with abs (gt 5)) (-5))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1147: "
{-# LINE 1147 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1147 "src/Test/HMock/Predicates.hs" #-}
      (accept (with reverse (eq "olleh")) "hello")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:1149: "
{-# LINE 1149 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1149 "src/Test/HMock/Predicates.hs" #-}
      (accept (with reverse (eq "olleh")) "goodbye")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1166: "
{-# LINE 1166 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1166 "src/Test/HMock/Predicates.hs" #-}
      (accept ($(qWith [| abs |]) (gt 5)) (-6))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:1168: "
{-# LINE 1168 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1168 "src/Test/HMock/Predicates.hs" #-}
      (accept ($(qWith [| abs |]) (gt 5)) (-5))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1170: "
{-# LINE 1170 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1170 "src/Test/HMock/Predicates.hs" #-}
      (accept ($(qWith [| reverse |]) (eq "olleh")) "hello")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:1172: "
{-# LINE 1172 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1172 "src/Test/HMock/Predicates.hs" #-}
      (accept ($(qWith [| reverse |]) (eq "olleh")) "goodbye")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1175: "
{-# LINE 1175 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1175 "src/Test/HMock/Predicates.hs" #-}
      (show ($(qWith [| abs |]) (gt 5)))
  [ExpectedLine [LineChunk "\"abs: > 5\""]]
 DocTest.printPrefix "Test.HMock.Predicates:1194: "
{-# LINE 1194 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1194 "src/Test/HMock/Predicates.hs" #-}
      (accept $(qMatch [p| Just (Left _) |]) Nothing)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1196: "
{-# LINE 1196 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1196 "src/Test/HMock/Predicates.hs" #-}
      (accept $(qMatch [p| Just (Left _) |]) (Just (Left 5)))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:1198: "
{-# LINE 1198 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1198 "src/Test/HMock/Predicates.hs" #-}
      (accept $(qMatch [p| Just (Left _) |]) (Just (Right 5)))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1201: "
{-# LINE 1201 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1201 "src/Test/HMock/Predicates.hs" #-}
      (show $(qMatch [p| Just (Left _) |]))
  [ExpectedLine [LineChunk "\"Just (Left _)\""]]
 DocTest.printPrefix "Test.HMock.Predicates:1223: "
{-# LINE 1223 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1223 "src/Test/HMock/Predicates.hs" #-}
      (accept (typed @String anything) "foo")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:1225: "
{-# LINE 1225 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1225 "src/Test/HMock/Predicates.hs" #-}
      (accept (typed @String (sizeIs (gt 5))) "foo")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1227: "
{-# LINE 1227 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1227 "src/Test/HMock/Predicates.hs" #-}
      (accept (typed @String anything) (42 :: Int))
  [ExpectedLine [LineChunk "False"]]
