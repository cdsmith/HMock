-- Do not edit! Automatically created with doctest-extract from src/Test/HMock/Predicates.hs
{-# LINE 102 "src/Test/HMock/Predicates.hs" #-}

{-# OPTIONS_GHC -XExtendedDefaultRules #-}
{-# OPTIONS_GHC -XTemplateHaskell #-}
{-# OPTIONS_GHC -XTypeApplications #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module DocTests.Test.HMock.Predicates where

import Test.HMock.Predicates
import Test.DocTest.Base
import qualified Test.DocTest.Driver as DocTest

{-# LINE 107 "src/Test/HMock/Predicates.hs" #-}

test :: DocTest.T ()
test = do
 DocTest.printPrefix "Test.HMock.Predicates:134: "
{-# LINE 134 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 134 "src/Test/HMock/Predicates.hs" #-}
      (accept anything "foo")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:136: "
{-# LINE 136 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 136 "src/Test/HMock/Predicates.hs" #-}
      (accept anything undefined)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:149: "
{-# LINE 149 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 149 "src/Test/HMock/Predicates.hs" #-}
      (accept (eq "foo") "foo")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:151: "
{-# LINE 151 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 151 "src/Test/HMock/Predicates.hs" #-}
      (accept (eq "foo") "bar")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:167: "
{-# LINE 167 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 167 "src/Test/HMock/Predicates.hs" #-}
      (accept (neq "foo") "foo")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:169: "
{-# LINE 169 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 169 "src/Test/HMock/Predicates.hs" #-}
      (accept (neq "foo") "bar")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:176: "
{-# LINE 176 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 176 "src/Test/HMock/Predicates.hs" #-}
      (accept (gt 5) 4)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:178: "
{-# LINE 178 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 178 "src/Test/HMock/Predicates.hs" #-}
      (accept (gt 5) 5)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:180: "
{-# LINE 180 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 180 "src/Test/HMock/Predicates.hs" #-}
      (accept (gt 5) 6)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:194: "
{-# LINE 194 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 194 "src/Test/HMock/Predicates.hs" #-}
      (accept (geq 5) 4)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:196: "
{-# LINE 196 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 196 "src/Test/HMock/Predicates.hs" #-}
      (accept (geq 5) 5)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:198: "
{-# LINE 198 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 198 "src/Test/HMock/Predicates.hs" #-}
      (accept (geq 5) 6)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:211: "
{-# LINE 211 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 211 "src/Test/HMock/Predicates.hs" #-}
      (accept (lt 5) 4)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:213: "
{-# LINE 213 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 213 "src/Test/HMock/Predicates.hs" #-}
      (accept (lt 5) 5)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:215: "
{-# LINE 215 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 215 "src/Test/HMock/Predicates.hs" #-}
      (accept (lt 5) 6)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:222: "
{-# LINE 222 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 222 "src/Test/HMock/Predicates.hs" #-}
      (accept (leq 5) 4)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:224: "
{-# LINE 224 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 224 "src/Test/HMock/Predicates.hs" #-}
      (accept (leq 5) 5)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:226: "
{-# LINE 226 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 226 "src/Test/HMock/Predicates.hs" #-}
      (accept (leq 5) 6)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:234: "
{-# LINE 234 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 234 "src/Test/HMock/Predicates.hs" #-}
      (accept (just (eq "value")) Nothing)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:236: "
{-# LINE 236 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 236 "src/Test/HMock/Predicates.hs" #-}
      (accept (just (eq "value")) (Just "value"))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:238: "
{-# LINE 238 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 238 "src/Test/HMock/Predicates.hs" #-}
      (accept (just (eq "value")) (Just "wrong value"))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:252: "
{-# LINE 252 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 252 "src/Test/HMock/Predicates.hs" #-}
      (accept nothing Nothing)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:255: "
{-# LINE 255 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 255 "src/Test/HMock/Predicates.hs" #-}
      (accept nothing (Just "something"))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:269: "
{-# LINE 269 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 269 "src/Test/HMock/Predicates.hs" #-}
      (accept (left (eq "value")) (Left "value"))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:271: "
{-# LINE 271 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 271 "src/Test/HMock/Predicates.hs" #-}
      (accept (left (eq "value")) (Right "value"))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:273: "
{-# LINE 273 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 273 "src/Test/HMock/Predicates.hs" #-}
      (accept (left (eq "value")) (Left "wrong value"))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:287: "
{-# LINE 287 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 287 "src/Test/HMock/Predicates.hs" #-}
      (accept (right (eq "value")) (Right "value"))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:289: "
{-# LINE 289 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 289 "src/Test/HMock/Predicates.hs" #-}
      (accept (right (eq "value")) (Right "wrong value"))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:291: "
{-# LINE 291 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 291 "src/Test/HMock/Predicates.hs" #-}
      (accept (right (eq "value")) (Left "value"))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:305: "
{-# LINE 305 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 305 "src/Test/HMock/Predicates.hs" #-}
      (accept (zipP (eq "foo") (eq "bar")) ("foo", "bar"))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:307: "
{-# LINE 307 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 307 "src/Test/HMock/Predicates.hs" #-}
      (accept (zipP (eq "foo") (eq "bar")) ("bar", "foo"))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:331: "
{-# LINE 331 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 331 "src/Test/HMock/Predicates.hs" #-}
      (accept (zip3P (eq "foo") (eq "bar") (eq "qux")) ("foo", "bar", "qux"))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:333: "
{-# LINE 333 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 333 "src/Test/HMock/Predicates.hs" #-}
      (accept (zip3P (eq "foo") (eq "bar") (eq "qux")) ("qux", "bar", "foo"))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:358: "
{-# LINE 358 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 358 "src/Test/HMock/Predicates.hs" #-}
      (accept (zip4P (eq 1) (eq 2) (eq 3) (eq 4)) (1, 2, 3, 4))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:360: "
{-# LINE 360 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 360 "src/Test/HMock/Predicates.hs" #-}
      (accept (zip4P (eq 1) (eq 2) (eq 3) (eq 4)) (4, 3, 2, 1))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:391: "
{-# LINE 391 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 391 "src/Test/HMock/Predicates.hs" #-}
      (accept (zip5P (eq 1) (eq 2) (eq 3) (eq 4) (eq 5)) (1, 2, 3, 4, 5))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:393: "
{-# LINE 393 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 393 "src/Test/HMock/Predicates.hs" #-}
      (accept (zip5P (eq 1) (eq 2) (eq 3) (eq 4) (eq 5)) (5, 4, 3, 2, 1))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:425: "
{-# LINE 425 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 425 "src/Test/HMock/Predicates.hs" #-}
      (accept (lt "foo" `andP` gt "bar") "eta")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:427: "
{-# LINE 427 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 427 "src/Test/HMock/Predicates.hs" #-}
      (accept (lt "foo" `andP` gt "bar") "quz")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:429: "
{-# LINE 429 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 429 "src/Test/HMock/Predicates.hs" #-}
      (accept (lt "foo" `andP` gt "bar") "alpha")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:446: "
{-# LINE 446 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 446 "src/Test/HMock/Predicates.hs" #-}
      (accept (lt "bar" `orP` gt "foo") "eta")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:448: "
{-# LINE 448 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 448 "src/Test/HMock/Predicates.hs" #-}
      (accept (lt "bar" `orP` gt "foo") "quz")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:450: "
{-# LINE 450 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 450 "src/Test/HMock/Predicates.hs" #-}
      (accept (lt "bar" `orP` gt "foo") "alpha")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:458: "
{-# LINE 458 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 458 "src/Test/HMock/Predicates.hs" #-}
      (accept (notP (eq "negative")) "positive")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:460: "
{-# LINE 460 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 460 "src/Test/HMock/Predicates.hs" #-}
      (accept (notP (eq "negative")) "negative")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:473: "
{-# LINE 473 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 473 "src/Test/HMock/Predicates.hs" #-}
      (accept (startsWith "fun") "fungible")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:475: "
{-# LINE 475 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 475 "src/Test/HMock/Predicates.hs" #-}
      (accept (startsWith "gib") "fungible")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:488: "
{-# LINE 488 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 488 "src/Test/HMock/Predicates.hs" #-}
      (accept (endsWith "ow") "crossbow")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:490: "
{-# LINE 490 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 490 "src/Test/HMock/Predicates.hs" #-}
      (accept (endsWith "ow") "trebuchet")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:504: "
{-# LINE 504 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 504 "src/Test/HMock/Predicates.hs" #-}
      (accept (hasSubstr "i") "team")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:506: "
{-# LINE 506 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 506 "src/Test/HMock/Predicates.hs" #-}
      (accept (hasSubstr "i") "partnership")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:520: "
{-# LINE 520 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 520 "src/Test/HMock/Predicates.hs" #-}
      (accept (hasSubsequence [1..5]) [1, 2, 3, 4, 5])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:522: "
{-# LINE 522 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 522 "src/Test/HMock/Predicates.hs" #-}
      (accept (hasSubsequence [1..5]) [0, 1, 0, 2, 0, 3, 0, 4, 0, 5, 0])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:524: "
{-# LINE 524 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 524 "src/Test/HMock/Predicates.hs" #-}
      (accept (hasSubsequence [1..5]) [2, 3, 5, 7, 11])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:538: "
{-# LINE 538 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 538 "src/Test/HMock/Predicates.hs" #-}
      (accept (caseInsensitive startsWith "foo") "FOOTBALL!")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:540: "
{-# LINE 540 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 540 "src/Test/HMock/Predicates.hs" #-}
      (accept (caseInsensitive endsWith "ball") "soccer")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:542: "
{-# LINE 542 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 542 "src/Test/HMock/Predicates.hs" #-}
      (accept (caseInsensitive eq "time") "TIME")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:544: "
{-# LINE 544 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 544 "src/Test/HMock/Predicates.hs" #-}
      (accept (caseInsensitive gt "NOTHING") "everything")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:571: "
{-# LINE 571 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 571 "src/Test/HMock/Predicates.hs" #-}
      (accept (matchesRegex "x{2,5}y?") "xxxy")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:573: "
{-# LINE 573 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 573 "src/Test/HMock/Predicates.hs" #-}
      (accept (matchesRegex "x{2,5}y?") "xyy")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:575: "
{-# LINE 575 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 575 "src/Test/HMock/Predicates.hs" #-}
      (accept (matchesRegex "x{2,5}y?") "wxxxyz")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:605: "
{-# LINE 605 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 605 "src/Test/HMock/Predicates.hs" #-}
      (accept (matchesCaseInsensitiveRegex "x{2,5}y?") "XXXY")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:607: "
{-# LINE 607 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 607 "src/Test/HMock/Predicates.hs" #-}
      (accept (matchesCaseInsensitiveRegex "x{2,5}y?") "XYY")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:609: "
{-# LINE 609 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 609 "src/Test/HMock/Predicates.hs" #-}
      (accept (matchesCaseInsensitiveRegex "x{2,5}y?") "WXXXYZ")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:645: "
{-# LINE 645 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 645 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsRegex "x{2,5}y?") "xxxy")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:647: "
{-# LINE 647 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 647 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsRegex "x{2,5}y?") "xyy")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:649: "
{-# LINE 649 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 649 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsRegex "x{2,5}y?") "wxxxyz")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:673: "
{-# LINE 673 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 673 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsCaseInsensitiveRegex "x{2,5}y?") "XXXY")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:675: "
{-# LINE 675 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 675 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsCaseInsensitiveRegex "x{2,5}y?") "XYY")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:677: "
{-# LINE 677 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 677 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsCaseInsensitiveRegex "x{2,5}y?") "WXXXYZ")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:701: "
{-# LINE 701 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 701 "src/Test/HMock/Predicates.hs" #-}
      (accept isEmpty ([] :: [Int]))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:703: "
{-# LINE 703 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 703 "src/Test/HMock/Predicates.hs" #-}
      (accept isEmpty [1, 2, 3])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:705: "
{-# LINE 705 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 705 "src/Test/HMock/Predicates.hs" #-}
      (accept isEmpty "")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:707: "
{-# LINE 707 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 707 "src/Test/HMock/Predicates.hs" #-}
      (accept isEmpty "gas tank")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:720: "
{-# LINE 720 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 720 "src/Test/HMock/Predicates.hs" #-}
      (accept nonEmpty ([] :: [Int]))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:722: "
{-# LINE 722 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 722 "src/Test/HMock/Predicates.hs" #-}
      (accept nonEmpty [1, 2, 3])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:724: "
{-# LINE 724 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 724 "src/Test/HMock/Predicates.hs" #-}
      (accept nonEmpty "")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:726: "
{-# LINE 726 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 726 "src/Test/HMock/Predicates.hs" #-}
      (accept nonEmpty "gas tank")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:734: "
{-# LINE 734 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 734 "src/Test/HMock/Predicates.hs" #-}
      (accept (sizeIs (lt 3)) ['a' .. 'f'])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:736: "
{-# LINE 736 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 736 "src/Test/HMock/Predicates.hs" #-}
      (accept (sizeIs (lt 3)) ['a' .. 'b'])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:757: "
{-# LINE 757 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 757 "src/Test/HMock/Predicates.hs" #-}
      (accept (elemsAre [lt 3, lt 4, lt 5]) [2, 3, 4])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:759: "
{-# LINE 759 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 759 "src/Test/HMock/Predicates.hs" #-}
      (accept (elemsAre [lt 3, lt 4, lt 5]) [2, 3, 4, 5])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:761: "
{-# LINE 761 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 761 "src/Test/HMock/Predicates.hs" #-}
      (accept (elemsAre [lt 3, lt 4, lt 5]) [2, 10, 4])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:793: "
{-# LINE 793 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 793 "src/Test/HMock/Predicates.hs" #-}
      (accept (unorderedElemsAre [eq 1, eq 2, eq 3]) [1, 2, 3])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:795: "
{-# LINE 795 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 795 "src/Test/HMock/Predicates.hs" #-}
      (accept (unorderedElemsAre [eq 1, eq 2, eq 3]) [2, 3, 1])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:797: "
{-# LINE 797 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 797 "src/Test/HMock/Predicates.hs" #-}
      (accept (unorderedElemsAre [eq 1, eq 2, eq 3]) [1, 2, 3, 4])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:799: "
{-# LINE 799 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 799 "src/Test/HMock/Predicates.hs" #-}
      (accept (unorderedElemsAre [eq 1, eq 2, eq 3]) [1, 3])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:846: "
{-# LINE 846 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 846 "src/Test/HMock/Predicates.hs" #-}
      (accept (each (gt 5)) [4, 5, 6])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:848: "
{-# LINE 848 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 848 "src/Test/HMock/Predicates.hs" #-}
      (accept (each (gt 5)) [6, 7, 8])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:850: "
{-# LINE 850 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 850 "src/Test/HMock/Predicates.hs" #-}
      (accept (each (gt 5)) [])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:875: "
{-# LINE 875 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 875 "src/Test/HMock/Predicates.hs" #-}
      (accept (contains (gt 5)) [3, 4, 5])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:877: "
{-# LINE 877 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 877 "src/Test/HMock/Predicates.hs" #-}
      (accept (contains (gt 5)) [4, 5, 6])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:879: "
{-# LINE 879 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 879 "src/Test/HMock/Predicates.hs" #-}
      (accept (contains (gt 5)) [])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:887: "
{-# LINE 887 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 887 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsAll [eq "foo", eq "bar"]) ["bar", "foo"])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:889: "
{-# LINE 889 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 889 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsAll [eq "foo", eq "bar"]) ["foo"])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:891: "
{-# LINE 891 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 891 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsAll [eq "foo", eq "bar"]) ["foo", "bar", "qux"])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:899: "
{-# LINE 899 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 899 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsAll [startsWith "f", endsWith "o"]) ["foo"])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:901: "
{-# LINE 901 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 901 "src/Test/HMock/Predicates.hs" #-}
      (accept (contains (startsWith "f") `andP` contains (endsWith "o")) ["foo"])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:923: "
{-# LINE 923 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 923 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsOnly [eq "foo", eq "bar"]) ["foo"])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:925: "
{-# LINE 925 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 925 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsOnly [eq "foo", eq "bar"]) ["foo", "bar"])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:927: "
{-# LINE 927 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 927 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsOnly [eq "foo", eq "bar"]) ["foo", "qux"])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:934: "
{-# LINE 934 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 934 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsOnly [eq "foo", eq "bar"]) ["foo", "foo"])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:936: "
{-# LINE 936 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 936 "src/Test/HMock/Predicates.hs" #-}
      (accept (each (eq "foo" `orP` eq "bar")) ["foo", "foo"])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:962: "
{-# LINE 962 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 962 "src/Test/HMock/Predicates.hs" #-}
      (accept (keys (each (eq "foo"))) [("foo", 5)])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:965: "
{-# LINE 965 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 965 "src/Test/HMock/Predicates.hs" #-}
      (accept (keys (each (eq "foo"))) [("foo", 5), ("bar", 6)])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:981: "
{-# LINE 981 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 981 "src/Test/HMock/Predicates.hs" #-}
      (accept (values (each (eq 5))) [("foo", 5), ("bar", 5)])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:984: "
{-# LINE 984 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 984 "src/Test/HMock/Predicates.hs" #-}
      (accept (values (each (eq 5))) [("foo", 5), ("bar", 6)])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1002: "
{-# LINE 1002 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1002 "src/Test/HMock/Predicates.hs" #-}
      (accept (eq 1.0) (sum (replicate 100 0.01)))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1009: "
{-# LINE 1009 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1009 "src/Test/HMock/Predicates.hs" #-}
      (accept (approxEq 1.0) (sum (replicate 100 0.01)))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:1011: "
{-# LINE 1011 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1011 "src/Test/HMock/Predicates.hs" #-}
      (accept (approxEq 1.0) (sum (replicate 100 0.009999)))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1026: "
{-# LINE 1026 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1026 "src/Test/HMock/Predicates.hs" #-}
      (accept positive 1)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:1029: "
{-# LINE 1029 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1029 "src/Test/HMock/Predicates.hs" #-}
      (accept positive 0)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1032: "
{-# LINE 1032 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1032 "src/Test/HMock/Predicates.hs" #-}
      (accept positive (-1))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1050: "
{-# LINE 1050 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1050 "src/Test/HMock/Predicates.hs" #-}
      (accept negative 1)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1053: "
{-# LINE 1053 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1053 "src/Test/HMock/Predicates.hs" #-}
      (accept negative 0)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1056: "
{-# LINE 1056 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1056 "src/Test/HMock/Predicates.hs" #-}
      (accept negative (-1))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:1075: "
{-# LINE 1075 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1075 "src/Test/HMock/Predicates.hs" #-}
      (accept nonPositive 1)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1078: "
{-# LINE 1078 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1078 "src/Test/HMock/Predicates.hs" #-}
      (accept nonPositive 0)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:1081: "
{-# LINE 1081 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1081 "src/Test/HMock/Predicates.hs" #-}
      (accept nonPositive (-1))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:1089: "
{-# LINE 1089 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1089 "src/Test/HMock/Predicates.hs" #-}
      (accept nonNegative 1)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:1092: "
{-# LINE 1092 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1092 "src/Test/HMock/Predicates.hs" #-}
      (accept nonNegative 0)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:1095: "
{-# LINE 1095 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1095 "src/Test/HMock/Predicates.hs" #-}
      (accept nonNegative (-1))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1102: "
{-# LINE 1102 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1102 "src/Test/HMock/Predicates.hs" #-}
      (accept finite 1.0)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:1104: "
{-# LINE 1104 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1104 "src/Test/HMock/Predicates.hs" #-}
      (accept finite (0 / 0))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1106: "
{-# LINE 1106 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1106 "src/Test/HMock/Predicates.hs" #-}
      (accept finite (1 / 0))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1124: "
{-# LINE 1124 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1124 "src/Test/HMock/Predicates.hs" #-}
      (accept infinite 1.0)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1126: "
{-# LINE 1126 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1126 "src/Test/HMock/Predicates.hs" #-}
      (accept infinite (0 / 0))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1128: "
{-# LINE 1128 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1128 "src/Test/HMock/Predicates.hs" #-}
      (accept infinite (1 / 0))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:1144: "
{-# LINE 1144 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1144 "src/Test/HMock/Predicates.hs" #-}
      (accept nAn 1.0)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1146: "
{-# LINE 1146 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1146 "src/Test/HMock/Predicates.hs" #-}
      (accept nAn (0 / 0))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:1148: "
{-# LINE 1148 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1148 "src/Test/HMock/Predicates.hs" #-}
      (accept nAn (1 / 0))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1166: "
{-# LINE 1166 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1166 "src/Test/HMock/Predicates.hs" #-}
      (accept (is even) 3)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1168: "
{-# LINE 1168 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1168 "src/Test/HMock/Predicates.hs" #-}
      (accept (is even) 4)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:1186: "
{-# LINE 1186 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1186 "src/Test/HMock/Predicates.hs" #-}
      (accept $(qIs [| even |]) 3)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1188: "
{-# LINE 1188 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1188 "src/Test/HMock/Predicates.hs" #-}
      (accept $(qIs [| even |]) 4)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:1191: "
{-# LINE 1191 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1191 "src/Test/HMock/Predicates.hs" #-}
      (show $(qIs [| even |]))
  [ExpectedLine [LineChunk "\"even\""]]
 DocTest.printPrefix "Test.HMock.Predicates:1209: "
{-# LINE 1209 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1209 "src/Test/HMock/Predicates.hs" #-}
      (accept (with abs (gt 5)) (-6))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:1211: "
{-# LINE 1211 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1211 "src/Test/HMock/Predicates.hs" #-}
      (accept (with abs (gt 5)) (-5))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1213: "
{-# LINE 1213 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1213 "src/Test/HMock/Predicates.hs" #-}
      (accept (with reverse (eq "olleh")) "hello")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:1215: "
{-# LINE 1215 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1215 "src/Test/HMock/Predicates.hs" #-}
      (accept (with reverse (eq "olleh")) "goodbye")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1232: "
{-# LINE 1232 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1232 "src/Test/HMock/Predicates.hs" #-}
      (accept ($(qWith [| abs |]) (gt 5)) (-6))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:1234: "
{-# LINE 1234 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1234 "src/Test/HMock/Predicates.hs" #-}
      (accept ($(qWith [| abs |]) (gt 5)) (-5))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1236: "
{-# LINE 1236 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1236 "src/Test/HMock/Predicates.hs" #-}
      (accept ($(qWith [| reverse |]) (eq "olleh")) "hello")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:1238: "
{-# LINE 1238 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1238 "src/Test/HMock/Predicates.hs" #-}
      (accept ($(qWith [| reverse |]) (eq "olleh")) "goodbye")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1241: "
{-# LINE 1241 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1241 "src/Test/HMock/Predicates.hs" #-}
      (show ($(qWith [| abs |]) (gt 5)))
  [ExpectedLine [LineChunk "\"abs: > 5\""]]
 DocTest.printPrefix "Test.HMock.Predicates:1260: "
{-# LINE 1260 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1260 "src/Test/HMock/Predicates.hs" #-}
      (accept $(qMatch [p| Just (Left _) |]) Nothing)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1262: "
{-# LINE 1262 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1262 "src/Test/HMock/Predicates.hs" #-}
      (accept $(qMatch [p| Just (Left _) |]) (Just (Left 5)))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:1264: "
{-# LINE 1264 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1264 "src/Test/HMock/Predicates.hs" #-}
      (accept $(qMatch [p| Just (Left _) |]) (Just (Right 5)))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1267: "
{-# LINE 1267 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1267 "src/Test/HMock/Predicates.hs" #-}
      (show $(qMatch [p| Just (Left _) |]))
  [ExpectedLine [LineChunk "\"Just (Left _)\""]]
 DocTest.printPrefix "Test.HMock.Predicates:1289: "
{-# LINE 1289 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1289 "src/Test/HMock/Predicates.hs" #-}
      (accept (typed @String anything) "foo")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:1291: "
{-# LINE 1291 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1291 "src/Test/HMock/Predicates.hs" #-}
      (accept (typed @String (sizeIs (gt 5))) "foo")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1293: "
{-# LINE 1293 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1293 "src/Test/HMock/Predicates.hs" #-}
      (accept (typed @String anything) (42 :: Int))
  [ExpectedLine [LineChunk "False"]]
