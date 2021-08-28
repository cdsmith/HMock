-- Do not edit! Automatically created with doctest-extract from src/Test/HMock/Predicates.hs
{-# LINE 101 "src/Test/HMock/Predicates.hs" #-}

{-# OPTIONS_GHC -XTemplateHaskell #-}
{-# OPTIONS_GHC -XTypeApplications #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module DocTests.Test.HMock.Predicates where

import Test.HMock.Predicates
import Test.DocTest.Base
import qualified Test.DocTest.Driver as DocTest

{-# LINE 105 "src/Test/HMock/Predicates.hs" #-}

test :: DocTest.T ()
test = do
 DocTest.printPrefix "Test.HMock.Predicates:132: "
{-# LINE 132 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 132 "src/Test/HMock/Predicates.hs" #-}
      (accept anything "foo")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:134: "
{-# LINE 134 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 134 "src/Test/HMock/Predicates.hs" #-}
      (accept anything undefined)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:147: "
{-# LINE 147 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 147 "src/Test/HMock/Predicates.hs" #-}
      (accept (eq "foo") "foo")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:149: "
{-# LINE 149 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 149 "src/Test/HMock/Predicates.hs" #-}
      (accept (eq "foo") "bar")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:165: "
{-# LINE 165 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 165 "src/Test/HMock/Predicates.hs" #-}
      (accept (neq "foo") "foo")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:167: "
{-# LINE 167 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 167 "src/Test/HMock/Predicates.hs" #-}
      (accept (neq "foo") "bar")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:174: "
{-# LINE 174 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 174 "src/Test/HMock/Predicates.hs" #-}
      (accept (gt 5) 4)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:176: "
{-# LINE 176 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 176 "src/Test/HMock/Predicates.hs" #-}
      (accept (gt 5) 5)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:178: "
{-# LINE 178 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 178 "src/Test/HMock/Predicates.hs" #-}
      (accept (gt 5) 6)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:192: "
{-# LINE 192 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 192 "src/Test/HMock/Predicates.hs" #-}
      (accept (geq 5) 4)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:194: "
{-# LINE 194 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 194 "src/Test/HMock/Predicates.hs" #-}
      (accept (geq 5) 5)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:196: "
{-# LINE 196 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 196 "src/Test/HMock/Predicates.hs" #-}
      (accept (geq 5) 6)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:209: "
{-# LINE 209 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 209 "src/Test/HMock/Predicates.hs" #-}
      (accept (lt 5) 4)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:211: "
{-# LINE 211 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 211 "src/Test/HMock/Predicates.hs" #-}
      (accept (lt 5) 5)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:213: "
{-# LINE 213 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 213 "src/Test/HMock/Predicates.hs" #-}
      (accept (lt 5) 6)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:220: "
{-# LINE 220 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 220 "src/Test/HMock/Predicates.hs" #-}
      (accept (leq 5) 4)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:222: "
{-# LINE 222 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 222 "src/Test/HMock/Predicates.hs" #-}
      (accept (leq 5) 5)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:224: "
{-# LINE 224 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 224 "src/Test/HMock/Predicates.hs" #-}
      (accept (leq 5) 6)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:232: "
{-# LINE 232 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 232 "src/Test/HMock/Predicates.hs" #-}
      (accept (just (eq "value")) Nothing)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:234: "
{-# LINE 234 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 234 "src/Test/HMock/Predicates.hs" #-}
      (accept (just (eq "value")) (Just "value"))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:236: "
{-# LINE 236 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 236 "src/Test/HMock/Predicates.hs" #-}
      (accept (just (eq "value")) (Just "wrong value"))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:250: "
{-# LINE 250 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 250 "src/Test/HMock/Predicates.hs" #-}
      (accept nothing Nothing)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:253: "
{-# LINE 253 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 253 "src/Test/HMock/Predicates.hs" #-}
      (accept nothing (Just "something"))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:267: "
{-# LINE 267 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 267 "src/Test/HMock/Predicates.hs" #-}
      (accept (left (eq "value")) (Left "value"))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:269: "
{-# LINE 269 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 269 "src/Test/HMock/Predicates.hs" #-}
      (accept (left (eq "value")) (Right "value"))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:271: "
{-# LINE 271 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 271 "src/Test/HMock/Predicates.hs" #-}
      (accept (left (eq "value")) (Left "wrong value"))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:285: "
{-# LINE 285 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 285 "src/Test/HMock/Predicates.hs" #-}
      (accept (right (eq "value")) (Right "value"))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:287: "
{-# LINE 287 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 287 "src/Test/HMock/Predicates.hs" #-}
      (accept (right (eq "value")) (Right "wrong value"))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:289: "
{-# LINE 289 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 289 "src/Test/HMock/Predicates.hs" #-}
      (accept (right (eq "value")) (Left "value"))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:303: "
{-# LINE 303 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 303 "src/Test/HMock/Predicates.hs" #-}
      (accept (zipP (eq "foo") (eq "bar")) ("foo", "bar"))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:305: "
{-# LINE 305 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 305 "src/Test/HMock/Predicates.hs" #-}
      (accept (zipP (eq "foo") (eq "bar")) ("bar", "foo"))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:329: "
{-# LINE 329 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 329 "src/Test/HMock/Predicates.hs" #-}
      (accept (zip3P (eq "foo") (eq "bar") (eq "qux")) ("foo", "bar", "qux"))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:331: "
{-# LINE 331 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 331 "src/Test/HMock/Predicates.hs" #-}
      (accept (zip3P (eq "foo") (eq "bar") (eq "qux")) ("qux", "bar", "foo"))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:356: "
{-# LINE 356 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 356 "src/Test/HMock/Predicates.hs" #-}
      (accept (zip4P (eq 1) (eq 2) (eq 3) (eq 4)) (1, 2, 3, 4))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:358: "
{-# LINE 358 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 358 "src/Test/HMock/Predicates.hs" #-}
      (accept (zip4P (eq 1) (eq 2) (eq 3) (eq 4)) (4, 3, 2, 1))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:389: "
{-# LINE 389 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 389 "src/Test/HMock/Predicates.hs" #-}
      (accept (zip5P (eq 1) (eq 2) (eq 3) (eq 4) (eq 5)) (1, 2, 3, 4, 5))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:391: "
{-# LINE 391 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 391 "src/Test/HMock/Predicates.hs" #-}
      (accept (zip5P (eq 1) (eq 2) (eq 3) (eq 4) (eq 5)) (5, 4, 3, 2, 1))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:423: "
{-# LINE 423 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 423 "src/Test/HMock/Predicates.hs" #-}
      (accept (lt "foo" `andP` gt "bar") "eta")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:425: "
{-# LINE 425 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 425 "src/Test/HMock/Predicates.hs" #-}
      (accept (lt "foo" `andP` gt "bar") "quz")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:427: "
{-# LINE 427 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 427 "src/Test/HMock/Predicates.hs" #-}
      (accept (lt "foo" `andP` gt "bar") "alpha")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:444: "
{-# LINE 444 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 444 "src/Test/HMock/Predicates.hs" #-}
      (accept (lt "bar" `orP` gt "foo") "eta")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:446: "
{-# LINE 446 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 446 "src/Test/HMock/Predicates.hs" #-}
      (accept (lt "bar" `orP` gt "foo") "quz")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:448: "
{-# LINE 448 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 448 "src/Test/HMock/Predicates.hs" #-}
      (accept (lt "bar" `orP` gt "foo") "alpha")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:456: "
{-# LINE 456 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 456 "src/Test/HMock/Predicates.hs" #-}
      (accept (notP (eq "negative")) "positive")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:458: "
{-# LINE 458 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 458 "src/Test/HMock/Predicates.hs" #-}
      (accept (notP (eq "negative")) "negative")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:471: "
{-# LINE 471 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 471 "src/Test/HMock/Predicates.hs" #-}
      (accept (startsWith "fun") "fungible")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:473: "
{-# LINE 473 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 473 "src/Test/HMock/Predicates.hs" #-}
      (accept (startsWith "gib") "fungible")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:486: "
{-# LINE 486 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 486 "src/Test/HMock/Predicates.hs" #-}
      (accept (endsWith "ow") "crossbow")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:488: "
{-# LINE 488 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 488 "src/Test/HMock/Predicates.hs" #-}
      (accept (endsWith "ow") "trebuchet")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:502: "
{-# LINE 502 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 502 "src/Test/HMock/Predicates.hs" #-}
      (accept (hasSubstr "i") "team")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:504: "
{-# LINE 504 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 504 "src/Test/HMock/Predicates.hs" #-}
      (accept (hasSubstr "i") "partnership")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:518: "
{-# LINE 518 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 518 "src/Test/HMock/Predicates.hs" #-}
      (accept (hasSubsequence [1..5]) [1, 2, 3, 4, 5])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:520: "
{-# LINE 520 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 520 "src/Test/HMock/Predicates.hs" #-}
      (accept (hasSubsequence [1..5]) [0, 1, 0, 2, 0, 3, 0, 4, 0, 5, 0])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:522: "
{-# LINE 522 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 522 "src/Test/HMock/Predicates.hs" #-}
      (accept (hasSubsequence [1..5]) [2, 3, 5, 7, 11])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:536: "
{-# LINE 536 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 536 "src/Test/HMock/Predicates.hs" #-}
      (accept (caseInsensitive startsWith "foo") "FOOTBALL!")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:538: "
{-# LINE 538 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 538 "src/Test/HMock/Predicates.hs" #-}
      (accept (caseInsensitive endsWith "ball") "soccer")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:540: "
{-# LINE 540 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 540 "src/Test/HMock/Predicates.hs" #-}
      (accept (caseInsensitive eq "time") "TIME")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:542: "
{-# LINE 542 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 542 "src/Test/HMock/Predicates.hs" #-}
      (accept (caseInsensitive gt "NOTHING") "everything")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:569: "
{-# LINE 569 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 569 "src/Test/HMock/Predicates.hs" #-}
      (accept (matchesRegex "x{2,5}y?") "xxxy")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:571: "
{-# LINE 571 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 571 "src/Test/HMock/Predicates.hs" #-}
      (accept (matchesRegex "x{2,5}y?") "xyy")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:573: "
{-# LINE 573 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 573 "src/Test/HMock/Predicates.hs" #-}
      (accept (matchesRegex "x{2,5}y?") "wxxxyz")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:603: "
{-# LINE 603 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 603 "src/Test/HMock/Predicates.hs" #-}
      (accept (matchesCaseInsensitiveRegex "x{2,5}y?") "XXXY")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:605: "
{-# LINE 605 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 605 "src/Test/HMock/Predicates.hs" #-}
      (accept (matchesCaseInsensitiveRegex "x{2,5}y?") "XYY")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:607: "
{-# LINE 607 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 607 "src/Test/HMock/Predicates.hs" #-}
      (accept (matchesCaseInsensitiveRegex "x{2,5}y?") "WXXXYZ")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:643: "
{-# LINE 643 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 643 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsRegex "x{2,5}y?") "xxxy")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:645: "
{-# LINE 645 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 645 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsRegex "x{2,5}y?") "xyy")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:647: "
{-# LINE 647 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 647 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsRegex "x{2,5}y?") "wxxxyz")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:671: "
{-# LINE 671 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 671 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsCaseInsensitiveRegex "x{2,5}y?") "XXXY")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:673: "
{-# LINE 673 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 673 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsCaseInsensitiveRegex "x{2,5}y?") "XYY")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:675: "
{-# LINE 675 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 675 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsCaseInsensitiveRegex "x{2,5}y?") "WXXXYZ")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:699: "
{-# LINE 699 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 699 "src/Test/HMock/Predicates.hs" #-}
      (accept isEmpty ([] :: [Int]))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:701: "
{-# LINE 701 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 701 "src/Test/HMock/Predicates.hs" #-}
      (accept isEmpty [1, 2, 3])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:703: "
{-# LINE 703 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 703 "src/Test/HMock/Predicates.hs" #-}
      (accept isEmpty "")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:705: "
{-# LINE 705 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 705 "src/Test/HMock/Predicates.hs" #-}
      (accept isEmpty "gas tank")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:718: "
{-# LINE 718 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 718 "src/Test/HMock/Predicates.hs" #-}
      (accept nonEmpty ([] :: [Int]))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:720: "
{-# LINE 720 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 720 "src/Test/HMock/Predicates.hs" #-}
      (accept nonEmpty [1, 2, 3])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:722: "
{-# LINE 722 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 722 "src/Test/HMock/Predicates.hs" #-}
      (accept nonEmpty "")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:724: "
{-# LINE 724 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 724 "src/Test/HMock/Predicates.hs" #-}
      (accept nonEmpty "gas tank")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:732: "
{-# LINE 732 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 732 "src/Test/HMock/Predicates.hs" #-}
      (accept (sizeIs (lt 3)) ['a' .. 'f'])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:734: "
{-# LINE 734 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 734 "src/Test/HMock/Predicates.hs" #-}
      (accept (sizeIs (lt 3)) ['a' .. 'b'])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:755: "
{-# LINE 755 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 755 "src/Test/HMock/Predicates.hs" #-}
      (accept (elemsAre [lt 3, lt 4, lt 5]) [2, 3, 4])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:757: "
{-# LINE 757 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 757 "src/Test/HMock/Predicates.hs" #-}
      (accept (elemsAre [lt 3, lt 4, lt 5]) [2, 3, 4, 5])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:759: "
{-# LINE 759 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 759 "src/Test/HMock/Predicates.hs" #-}
      (accept (elemsAre [lt 3, lt 4, lt 5]) [2, 10, 4])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:791: "
{-# LINE 791 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 791 "src/Test/HMock/Predicates.hs" #-}
      (accept (unorderedElemsAre [eq 1, eq 2, eq 3]) [1, 2, 3])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:793: "
{-# LINE 793 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 793 "src/Test/HMock/Predicates.hs" #-}
      (accept (unorderedElemsAre [eq 1, eq 2, eq 3]) [2, 3, 1])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:795: "
{-# LINE 795 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 795 "src/Test/HMock/Predicates.hs" #-}
      (accept (unorderedElemsAre [eq 1, eq 2, eq 3]) [1, 2, 3, 4])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:797: "
{-# LINE 797 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 797 "src/Test/HMock/Predicates.hs" #-}
      (accept (unorderedElemsAre [eq 1, eq 2, eq 3]) [1, 3])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:844: "
{-# LINE 844 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 844 "src/Test/HMock/Predicates.hs" #-}
      (accept (each (gt 5)) [4, 5, 6])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:846: "
{-# LINE 846 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 846 "src/Test/HMock/Predicates.hs" #-}
      (accept (each (gt 5)) [6, 7, 8])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:848: "
{-# LINE 848 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 848 "src/Test/HMock/Predicates.hs" #-}
      (accept (each (gt 5)) [])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:873: "
{-# LINE 873 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 873 "src/Test/HMock/Predicates.hs" #-}
      (accept (contains (gt 5)) [3, 4, 5])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:875: "
{-# LINE 875 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 875 "src/Test/HMock/Predicates.hs" #-}
      (accept (contains (gt 5)) [4, 5, 6])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:877: "
{-# LINE 877 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 877 "src/Test/HMock/Predicates.hs" #-}
      (accept (contains (gt 5)) [])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:885: "
{-# LINE 885 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 885 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsAll [eq "foo", eq "bar"]) ["bar", "foo"])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:887: "
{-# LINE 887 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 887 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsAll [eq "foo", eq "bar"]) ["foo"])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:889: "
{-# LINE 889 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 889 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsAll [eq "foo", eq "bar"]) ["foo", "bar", "qux"])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:897: "
{-# LINE 897 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 897 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsAll [startsWith "f", endsWith "o"]) ["foo"])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:899: "
{-# LINE 899 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 899 "src/Test/HMock/Predicates.hs" #-}
      (accept (contains (startsWith "f") `andP` contains (endsWith "o")) ["foo"])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:921: "
{-# LINE 921 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 921 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsOnly [eq "foo", eq "bar"]) ["foo"])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:923: "
{-# LINE 923 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 923 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsOnly [eq "foo", eq "bar"]) ["foo", "bar"])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:925: "
{-# LINE 925 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 925 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsOnly [eq "foo", eq "bar"]) ["foo", "qux"])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:932: "
{-# LINE 932 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 932 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsOnly [eq "foo", eq "bar"]) ["foo", "foo"])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:934: "
{-# LINE 934 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 934 "src/Test/HMock/Predicates.hs" #-}
      (accept (each (eq "foo" `orP` eq "bar")) ["foo", "foo"])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:960: "
{-# LINE 960 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 960 "src/Test/HMock/Predicates.hs" #-}
      (accept (keys (each (eq "foo"))) [("foo", 5)])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:963: "
{-# LINE 963 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 963 "src/Test/HMock/Predicates.hs" #-}
      (accept (keys (each (eq "foo"))) [("foo", 5), ("bar", 6)])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:979: "
{-# LINE 979 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 979 "src/Test/HMock/Predicates.hs" #-}
      (accept (values (each (eq 5))) [("foo", 5), ("bar", 5)])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:982: "
{-# LINE 982 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 982 "src/Test/HMock/Predicates.hs" #-}
      (accept (values (each (eq 5))) [("foo", 5), ("bar", 6)])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1000: "
{-# LINE 1000 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1000 "src/Test/HMock/Predicates.hs" #-}
      (accept (eq 1.0) (sum (replicate 100 0.01)))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1007: "
{-# LINE 1007 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1007 "src/Test/HMock/Predicates.hs" #-}
      (accept (approxEq 1.0) (sum (replicate 100 0.01)))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:1009: "
{-# LINE 1009 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1009 "src/Test/HMock/Predicates.hs" #-}
      (accept (approxEq 1.0) (sum (replicate 100 0.009999)))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1024: "
{-# LINE 1024 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1024 "src/Test/HMock/Predicates.hs" #-}
      (accept positive 1)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:1027: "
{-# LINE 1027 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1027 "src/Test/HMock/Predicates.hs" #-}
      (accept positive 0)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1030: "
{-# LINE 1030 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1030 "src/Test/HMock/Predicates.hs" #-}
      (accept positive (-1))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1048: "
{-# LINE 1048 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1048 "src/Test/HMock/Predicates.hs" #-}
      (accept negative 1)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1051: "
{-# LINE 1051 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1051 "src/Test/HMock/Predicates.hs" #-}
      (accept negative 0)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1054: "
{-# LINE 1054 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1054 "src/Test/HMock/Predicates.hs" #-}
      (accept negative (-1))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:1073: "
{-# LINE 1073 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1073 "src/Test/HMock/Predicates.hs" #-}
      (accept nonPositive 1)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1076: "
{-# LINE 1076 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1076 "src/Test/HMock/Predicates.hs" #-}
      (accept nonPositive 0)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:1079: "
{-# LINE 1079 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1079 "src/Test/HMock/Predicates.hs" #-}
      (accept nonPositive (-1))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:1087: "
{-# LINE 1087 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1087 "src/Test/HMock/Predicates.hs" #-}
      (accept nonNegative 1)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:1090: "
{-# LINE 1090 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1090 "src/Test/HMock/Predicates.hs" #-}
      (accept nonNegative 0)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:1093: "
{-# LINE 1093 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1093 "src/Test/HMock/Predicates.hs" #-}
      (accept nonNegative (-1))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1100: "
{-# LINE 1100 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1100 "src/Test/HMock/Predicates.hs" #-}
      (accept finite 1.0)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:1102: "
{-# LINE 1102 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1102 "src/Test/HMock/Predicates.hs" #-}
      (accept finite (0 / 0))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1104: "
{-# LINE 1104 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1104 "src/Test/HMock/Predicates.hs" #-}
      (accept finite (1 / 0))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1122: "
{-# LINE 1122 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1122 "src/Test/HMock/Predicates.hs" #-}
      (accept infinite 1.0)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1124: "
{-# LINE 1124 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1124 "src/Test/HMock/Predicates.hs" #-}
      (accept infinite (0 / 0))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1126: "
{-# LINE 1126 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1126 "src/Test/HMock/Predicates.hs" #-}
      (accept infinite (1 / 0))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:1142: "
{-# LINE 1142 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1142 "src/Test/HMock/Predicates.hs" #-}
      (accept nAn 1.0)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1144: "
{-# LINE 1144 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1144 "src/Test/HMock/Predicates.hs" #-}
      (accept nAn (0 / 0))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:1146: "
{-# LINE 1146 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1146 "src/Test/HMock/Predicates.hs" #-}
      (accept nAn (1 / 0))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1164: "
{-# LINE 1164 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1164 "src/Test/HMock/Predicates.hs" #-}
      (accept (is even) 3)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1166: "
{-# LINE 1166 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1166 "src/Test/HMock/Predicates.hs" #-}
      (accept (is even) 4)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:1184: "
{-# LINE 1184 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1184 "src/Test/HMock/Predicates.hs" #-}
      (accept $(qIs [| even |]) 3)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1186: "
{-# LINE 1186 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1186 "src/Test/HMock/Predicates.hs" #-}
      (accept $(qIs [| even |]) 4)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:1189: "
{-# LINE 1189 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1189 "src/Test/HMock/Predicates.hs" #-}
      (show $(qIs [| even |]))
  [ExpectedLine [LineChunk "\"even\""]]
 DocTest.printPrefix "Test.HMock.Predicates:1207: "
{-# LINE 1207 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1207 "src/Test/HMock/Predicates.hs" #-}
      (accept (with abs (gt 5)) (-6))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:1209: "
{-# LINE 1209 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1209 "src/Test/HMock/Predicates.hs" #-}
      (accept (with abs (gt 5)) (-5))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1211: "
{-# LINE 1211 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1211 "src/Test/HMock/Predicates.hs" #-}
      (accept (with reverse (eq "olleh")) "hello")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:1213: "
{-# LINE 1213 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1213 "src/Test/HMock/Predicates.hs" #-}
      (accept (with reverse (eq "olleh")) "goodbye")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1230: "
{-# LINE 1230 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1230 "src/Test/HMock/Predicates.hs" #-}
      (accept ($(qWith [| abs |]) (gt 5)) (-6))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:1232: "
{-# LINE 1232 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1232 "src/Test/HMock/Predicates.hs" #-}
      (accept ($(qWith [| abs |]) (gt 5)) (-5))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1234: "
{-# LINE 1234 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1234 "src/Test/HMock/Predicates.hs" #-}
      (accept ($(qWith [| reverse |]) (eq "olleh")) "hello")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:1236: "
{-# LINE 1236 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1236 "src/Test/HMock/Predicates.hs" #-}
      (accept ($(qWith [| reverse |]) (eq "olleh")) "goodbye")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1239: "
{-# LINE 1239 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1239 "src/Test/HMock/Predicates.hs" #-}
      (show ($(qWith [| abs |]) (gt 5)))
  [ExpectedLine [LineChunk "\"abs: > 5\""]]
 DocTest.printPrefix "Test.HMock.Predicates:1258: "
{-# LINE 1258 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1258 "src/Test/HMock/Predicates.hs" #-}
      (accept $(qMatch [p| Just (Left _) |]) Nothing)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1260: "
{-# LINE 1260 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1260 "src/Test/HMock/Predicates.hs" #-}
      (accept $(qMatch [p| Just (Left _) |]) (Just (Left 5)))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:1262: "
{-# LINE 1262 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1262 "src/Test/HMock/Predicates.hs" #-}
      (accept $(qMatch [p| Just (Left _) |]) (Just (Right 5)))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1265: "
{-# LINE 1265 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1265 "src/Test/HMock/Predicates.hs" #-}
      (show $(qMatch [p| Just (Left _) |]))
  [ExpectedLine [LineChunk "\"Just (Left _)\""]]
 DocTest.printPrefix "Test.HMock.Predicates:1287: "
{-# LINE 1287 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1287 "src/Test/HMock/Predicates.hs" #-}
      (accept (typed @String anything) "foo")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:1289: "
{-# LINE 1289 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1289 "src/Test/HMock/Predicates.hs" #-}
      (accept (typed @String (sizeIs (gt 5))) "foo")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1291: "
{-# LINE 1291 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1291 "src/Test/HMock/Predicates.hs" #-}
      (accept (typed @String anything) (42 :: Int))
  [ExpectedLine [LineChunk "False"]]
