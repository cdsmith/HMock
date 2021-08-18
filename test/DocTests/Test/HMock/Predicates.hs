-- Do not edit! Automatically created with doctest-extract from src/Test/HMock/Predicates.hs
{-# LINE 85 "src/Test/HMock/Predicates.hs" #-}

{-# OPTIONS_GHC -XTemplateHaskell #-}
{-# OPTIONS_GHC -XTypeApplications #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module DocTests.Test.HMock.Predicates where

import Test.HMock.Predicates
import Test.DocTest.Base
import qualified Test.DocTest.Driver as DocTest

{-# LINE 89 "src/Test/HMock/Predicates.hs" #-}

test :: DocTest.T ()
test = do
 DocTest.printPrefix "Test.HMock.Predicates:116: "
{-# LINE 116 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 116 "src/Test/HMock/Predicates.hs" #-}
      (accept anything "foo")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:118: "
{-# LINE 118 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 118 "src/Test/HMock/Predicates.hs" #-}
      (accept anything undefined)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:131: "
{-# LINE 131 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 131 "src/Test/HMock/Predicates.hs" #-}
      (accept (eq "foo") "foo")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:133: "
{-# LINE 133 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 133 "src/Test/HMock/Predicates.hs" #-}
      (accept (eq "foo") "bar")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:149: "
{-# LINE 149 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 149 "src/Test/HMock/Predicates.hs" #-}
      (accept (neq "foo") "foo")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:151: "
{-# LINE 151 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 151 "src/Test/HMock/Predicates.hs" #-}
      (accept (neq "foo") "bar")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:158: "
{-# LINE 158 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 158 "src/Test/HMock/Predicates.hs" #-}
      (accept (gt 5) 4)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:160: "
{-# LINE 160 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 160 "src/Test/HMock/Predicates.hs" #-}
      (accept (gt 5) 5)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:162: "
{-# LINE 162 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 162 "src/Test/HMock/Predicates.hs" #-}
      (accept (gt 5) 6)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:176: "
{-# LINE 176 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 176 "src/Test/HMock/Predicates.hs" #-}
      (accept (geq 5) 4)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:178: "
{-# LINE 178 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 178 "src/Test/HMock/Predicates.hs" #-}
      (accept (geq 5) 5)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:180: "
{-# LINE 180 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 180 "src/Test/HMock/Predicates.hs" #-}
      (accept (geq 5) 6)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:193: "
{-# LINE 193 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 193 "src/Test/HMock/Predicates.hs" #-}
      (accept (lt 5) 4)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:195: "
{-# LINE 195 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 195 "src/Test/HMock/Predicates.hs" #-}
      (accept (lt 5) 5)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:197: "
{-# LINE 197 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 197 "src/Test/HMock/Predicates.hs" #-}
      (accept (lt 5) 6)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:204: "
{-# LINE 204 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 204 "src/Test/HMock/Predicates.hs" #-}
      (accept (leq 5) 4)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:206: "
{-# LINE 206 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 206 "src/Test/HMock/Predicates.hs" #-}
      (accept (leq 5) 5)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:208: "
{-# LINE 208 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 208 "src/Test/HMock/Predicates.hs" #-}
      (accept (leq 5) 6)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:216: "
{-# LINE 216 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 216 "src/Test/HMock/Predicates.hs" #-}
      (accept (just (eq "value")) Nothing)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:218: "
{-# LINE 218 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 218 "src/Test/HMock/Predicates.hs" #-}
      (accept (just (eq "value")) (Just "value"))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:220: "
{-# LINE 220 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 220 "src/Test/HMock/Predicates.hs" #-}
      (accept (just (eq "value")) (Just "wrong value"))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:234: "
{-# LINE 234 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 234 "src/Test/HMock/Predicates.hs" #-}
      (accept nothing Nothing)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:237: "
{-# LINE 237 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 237 "src/Test/HMock/Predicates.hs" #-}
      (accept nothing (Just "something"))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:251: "
{-# LINE 251 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 251 "src/Test/HMock/Predicates.hs" #-}
      (accept (left (eq "value")) (Left "value"))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:253: "
{-# LINE 253 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 253 "src/Test/HMock/Predicates.hs" #-}
      (accept (left (eq "value")) (Right "value"))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:255: "
{-# LINE 255 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 255 "src/Test/HMock/Predicates.hs" #-}
      (accept (left (eq "value")) (Left "wrong value"))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:269: "
{-# LINE 269 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 269 "src/Test/HMock/Predicates.hs" #-}
      (accept (right (eq "value")) (Right "value"))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:271: "
{-# LINE 271 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 271 "src/Test/HMock/Predicates.hs" #-}
      (accept (right (eq "value")) (Right "wrong value"))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:273: "
{-# LINE 273 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 273 "src/Test/HMock/Predicates.hs" #-}
      (accept (right (eq "value")) (Left "value"))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:287: "
{-# LINE 287 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 287 "src/Test/HMock/Predicates.hs" #-}
      (accept (zipP (eq "foo") (eq "bar")) ("foo", "bar"))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:289: "
{-# LINE 289 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 289 "src/Test/HMock/Predicates.hs" #-}
      (accept (zipP (eq "foo") (eq "bar")) ("bar", "foo"))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:313: "
{-# LINE 313 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 313 "src/Test/HMock/Predicates.hs" #-}
      (accept (zip3P (eq "foo") (eq "bar") (eq "qux")) ("foo", "bar", "qux"))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:315: "
{-# LINE 315 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 315 "src/Test/HMock/Predicates.hs" #-}
      (accept (zip3P (eq "foo") (eq "bar") (eq "qux")) ("qux", "bar", "foo"))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:340: "
{-# LINE 340 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 340 "src/Test/HMock/Predicates.hs" #-}
      (accept (zip4P (eq 1) (eq 2) (eq 3) (eq 4)) (1, 2, 3, 4))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:342: "
{-# LINE 342 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 342 "src/Test/HMock/Predicates.hs" #-}
      (accept (zip4P (eq 1) (eq 2) (eq 3) (eq 4)) (4, 3, 2, 1))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:373: "
{-# LINE 373 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 373 "src/Test/HMock/Predicates.hs" #-}
      (accept (zip5P (eq 1) (eq 2) (eq 3) (eq 4) (eq 5)) (1, 2, 3, 4, 5))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:375: "
{-# LINE 375 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 375 "src/Test/HMock/Predicates.hs" #-}
      (accept (zip5P (eq 1) (eq 2) (eq 3) (eq 4) (eq 5)) (5, 4, 3, 2, 1))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:407: "
{-# LINE 407 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 407 "src/Test/HMock/Predicates.hs" #-}
      (accept (lt "foo" `andP` gt "bar") "eta")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:409: "
{-# LINE 409 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 409 "src/Test/HMock/Predicates.hs" #-}
      (accept (lt "foo" `andP` gt "bar") "quz")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:411: "
{-# LINE 411 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 411 "src/Test/HMock/Predicates.hs" #-}
      (accept (lt "foo" `andP` gt "bar") "alpha")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:428: "
{-# LINE 428 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 428 "src/Test/HMock/Predicates.hs" #-}
      (accept (lt "bar" `orP` gt "foo") "eta")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:430: "
{-# LINE 430 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 430 "src/Test/HMock/Predicates.hs" #-}
      (accept (lt "bar" `orP` gt "foo") "quz")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:432: "
{-# LINE 432 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 432 "src/Test/HMock/Predicates.hs" #-}
      (accept (lt "bar" `orP` gt "foo") "alpha")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:440: "
{-# LINE 440 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 440 "src/Test/HMock/Predicates.hs" #-}
      (accept (notP (eq "negative")) "positive")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:442: "
{-# LINE 442 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 442 "src/Test/HMock/Predicates.hs" #-}
      (accept (notP (eq "negative")) "negative")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:455: "
{-# LINE 455 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 455 "src/Test/HMock/Predicates.hs" #-}
      (accept (startsWith "fun") "fungible")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:457: "
{-# LINE 457 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 457 "src/Test/HMock/Predicates.hs" #-}
      (accept (startsWith "gib") "fungible")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:470: "
{-# LINE 470 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 470 "src/Test/HMock/Predicates.hs" #-}
      (accept (endsWith "ow") "crossbow")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:472: "
{-# LINE 472 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 472 "src/Test/HMock/Predicates.hs" #-}
      (accept (endsWith "ow") "trebuchet")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:486: "
{-# LINE 486 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 486 "src/Test/HMock/Predicates.hs" #-}
      (accept (hasSubstr "i") "team")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:488: "
{-# LINE 488 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 488 "src/Test/HMock/Predicates.hs" #-}
      (accept (hasSubstr "i") "partnership")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:502: "
{-# LINE 502 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 502 "src/Test/HMock/Predicates.hs" #-}
      (accept (hasSubsequence [1..5]) [1, 2, 3, 4, 5])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:504: "
{-# LINE 504 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 504 "src/Test/HMock/Predicates.hs" #-}
      (accept (hasSubsequence [1..5]) [0, 1, 0, 2, 0, 3, 0, 4, 0, 5, 0])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:506: "
{-# LINE 506 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 506 "src/Test/HMock/Predicates.hs" #-}
      (accept (hasSubsequence [1..5]) [2, 3, 5, 7, 11])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:520: "
{-# LINE 520 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 520 "src/Test/HMock/Predicates.hs" #-}
      (accept (caseInsensitive startsWith "foo") "FOOTBALL!")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:522: "
{-# LINE 522 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 522 "src/Test/HMock/Predicates.hs" #-}
      (accept (caseInsensitive endsWith "ball") "soccer")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:524: "
{-# LINE 524 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 524 "src/Test/HMock/Predicates.hs" #-}
      (accept (caseInsensitive eq "time") "TIME")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:526: "
{-# LINE 526 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 526 "src/Test/HMock/Predicates.hs" #-}
      (accept (caseInsensitive gt "NOTHING") "everything")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:553: "
{-# LINE 553 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 553 "src/Test/HMock/Predicates.hs" #-}
      (accept (matchesRegex "x{2,5}y?") "xxxy")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:555: "
{-# LINE 555 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 555 "src/Test/HMock/Predicates.hs" #-}
      (accept (matchesRegex "x{2,5}y?") "xyy")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:557: "
{-# LINE 557 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 557 "src/Test/HMock/Predicates.hs" #-}
      (accept (matchesRegex "x{2,5}y?") "wxxxyz")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:587: "
{-# LINE 587 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 587 "src/Test/HMock/Predicates.hs" #-}
      (accept (matchesCaseInsensitiveRegex "x{2,5}y?") "XXXY")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:589: "
{-# LINE 589 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 589 "src/Test/HMock/Predicates.hs" #-}
      (accept (matchesCaseInsensitiveRegex "x{2,5}y?") "XYY")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:591: "
{-# LINE 591 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 591 "src/Test/HMock/Predicates.hs" #-}
      (accept (matchesCaseInsensitiveRegex "x{2,5}y?") "WXXXYZ")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:627: "
{-# LINE 627 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 627 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsRegex "x{2,5}y?") "xxxy")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:629: "
{-# LINE 629 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 629 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsRegex "x{2,5}y?") "xyy")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:631: "
{-# LINE 631 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 631 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsRegex "x{2,5}y?") "wxxxyz")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:655: "
{-# LINE 655 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 655 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsCaseInsensitiveRegex "x{2,5}y?") "XXXY")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:657: "
{-# LINE 657 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 657 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsCaseInsensitiveRegex "x{2,5}y?") "XYY")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:659: "
{-# LINE 659 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 659 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsCaseInsensitiveRegex "x{2,5}y?") "WXXXYZ")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:683: "
{-# LINE 683 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 683 "src/Test/HMock/Predicates.hs" #-}
      (accept isEmpty ([] :: [Int]))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:685: "
{-# LINE 685 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 685 "src/Test/HMock/Predicates.hs" #-}
      (accept isEmpty [1, 2, 3])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:687: "
{-# LINE 687 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 687 "src/Test/HMock/Predicates.hs" #-}
      (accept isEmpty "")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:689: "
{-# LINE 689 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 689 "src/Test/HMock/Predicates.hs" #-}
      (accept isEmpty "gas tank")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:702: "
{-# LINE 702 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 702 "src/Test/HMock/Predicates.hs" #-}
      (accept nonEmpty ([] :: [Int]))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:704: "
{-# LINE 704 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 704 "src/Test/HMock/Predicates.hs" #-}
      (accept nonEmpty [1, 2, 3])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:706: "
{-# LINE 706 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 706 "src/Test/HMock/Predicates.hs" #-}
      (accept nonEmpty "")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:708: "
{-# LINE 708 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 708 "src/Test/HMock/Predicates.hs" #-}
      (accept nonEmpty "gas tank")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:716: "
{-# LINE 716 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 716 "src/Test/HMock/Predicates.hs" #-}
      (accept (sizeIs (lt 3)) ['a' .. 'f'])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:718: "
{-# LINE 718 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 718 "src/Test/HMock/Predicates.hs" #-}
      (accept (sizeIs (lt 3)) ['a' .. 'b'])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:739: "
{-# LINE 739 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 739 "src/Test/HMock/Predicates.hs" #-}
      (accept (elemsAre [lt 3, lt 4, lt 5]) [2, 3, 4])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:741: "
{-# LINE 741 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 741 "src/Test/HMock/Predicates.hs" #-}
      (accept (elemsAre [lt 3, lt 4, lt 5]) [2, 3, 4, 5])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:743: "
{-# LINE 743 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 743 "src/Test/HMock/Predicates.hs" #-}
      (accept (elemsAre [lt 3, lt 4, lt 5]) [2, 10, 4])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:775: "
{-# LINE 775 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 775 "src/Test/HMock/Predicates.hs" #-}
      (accept (unorderedElemsAre [eq 1, eq 2, eq 3]) [1, 2, 3])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:777: "
{-# LINE 777 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 777 "src/Test/HMock/Predicates.hs" #-}
      (accept (unorderedElemsAre [eq 1, eq 2, eq 3]) [2, 3, 1])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:779: "
{-# LINE 779 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 779 "src/Test/HMock/Predicates.hs" #-}
      (accept (unorderedElemsAre [eq 1, eq 2, eq 3]) [1, 2, 3, 4])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:781: "
{-# LINE 781 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 781 "src/Test/HMock/Predicates.hs" #-}
      (accept (unorderedElemsAre [eq 1, eq 2, eq 3]) [1, 3])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:828: "
{-# LINE 828 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 828 "src/Test/HMock/Predicates.hs" #-}
      (accept (each (gt 5)) [4, 5, 6])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:830: "
{-# LINE 830 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 830 "src/Test/HMock/Predicates.hs" #-}
      (accept (each (gt 5)) [6, 7, 8])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:832: "
{-# LINE 832 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 832 "src/Test/HMock/Predicates.hs" #-}
      (accept (each (gt 5)) [])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:857: "
{-# LINE 857 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 857 "src/Test/HMock/Predicates.hs" #-}
      (accept (contains (gt 5)) [3, 4, 5])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:859: "
{-# LINE 859 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 859 "src/Test/HMock/Predicates.hs" #-}
      (accept (contains (gt 5)) [4, 5, 6])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:861: "
{-# LINE 861 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 861 "src/Test/HMock/Predicates.hs" #-}
      (accept (contains (gt 5)) [])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:869: "
{-# LINE 869 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 869 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsAll [eq "foo", eq "bar"]) ["bar", "foo"])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:871: "
{-# LINE 871 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 871 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsAll [eq "foo", eq "bar"]) ["foo"])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:873: "
{-# LINE 873 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 873 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsAll [eq "foo", eq "bar"]) ["foo", "bar", "qux"])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:881: "
{-# LINE 881 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 881 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsAll [startsWith "f", endsWith "o"]) ["foo"])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:883: "
{-# LINE 883 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 883 "src/Test/HMock/Predicates.hs" #-}
      (accept (contains (startsWith "f") `andP` contains (endsWith "o")) ["foo"])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:905: "
{-# LINE 905 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 905 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsOnly [eq "foo", eq "bar"]) ["foo"])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:907: "
{-# LINE 907 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 907 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsOnly [eq "foo", eq "bar"]) ["foo", "bar"])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:909: "
{-# LINE 909 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 909 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsOnly [eq "foo", eq "bar"]) ["foo", "qux"])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:916: "
{-# LINE 916 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 916 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsOnly [eq "foo", eq "bar"]) ["foo", "foo"])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:918: "
{-# LINE 918 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 918 "src/Test/HMock/Predicates.hs" #-}
      (accept (each (eq "foo" `orP` eq "bar")) ["foo", "foo"])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:944: "
{-# LINE 944 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 944 "src/Test/HMock/Predicates.hs" #-}
      (accept (keys (each (eq "foo"))) [("foo", 5)])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:947: "
{-# LINE 947 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 947 "src/Test/HMock/Predicates.hs" #-}
      (accept (keys (each (eq "foo"))) [("foo", 5), ("bar", 6)])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:963: "
{-# LINE 963 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 963 "src/Test/HMock/Predicates.hs" #-}
      (accept (values (each (eq 5))) [("foo", 5), ("bar", 5)])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:966: "
{-# LINE 966 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 966 "src/Test/HMock/Predicates.hs" #-}
      (accept (values (each (eq 5))) [("foo", 5), ("bar", 6)])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:984: "
{-# LINE 984 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 984 "src/Test/HMock/Predicates.hs" #-}
      (accept (eq 1.0) (sum (replicate 100 0.01)))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:991: "
{-# LINE 991 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 991 "src/Test/HMock/Predicates.hs" #-}
      (accept (approxEq 1.0) (sum (replicate 100 0.01)))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:993: "
{-# LINE 993 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 993 "src/Test/HMock/Predicates.hs" #-}
      (accept (approxEq 1.0) (sum (replicate 100 0.009999)))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1008: "
{-# LINE 1008 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1008 "src/Test/HMock/Predicates.hs" #-}
      (accept positive 1)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:1011: "
{-# LINE 1011 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1011 "src/Test/HMock/Predicates.hs" #-}
      (accept positive 0)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1014: "
{-# LINE 1014 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1014 "src/Test/HMock/Predicates.hs" #-}
      (accept positive (-1))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1032: "
{-# LINE 1032 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1032 "src/Test/HMock/Predicates.hs" #-}
      (accept negative 1)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1035: "
{-# LINE 1035 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1035 "src/Test/HMock/Predicates.hs" #-}
      (accept negative 0)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1038: "
{-# LINE 1038 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1038 "src/Test/HMock/Predicates.hs" #-}
      (accept negative (-1))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:1057: "
{-# LINE 1057 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1057 "src/Test/HMock/Predicates.hs" #-}
      (accept nonPositive 1)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1060: "
{-# LINE 1060 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1060 "src/Test/HMock/Predicates.hs" #-}
      (accept nonPositive 0)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:1063: "
{-# LINE 1063 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1063 "src/Test/HMock/Predicates.hs" #-}
      (accept nonPositive (-1))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:1071: "
{-# LINE 1071 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1071 "src/Test/HMock/Predicates.hs" #-}
      (accept nonNegative 1)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:1074: "
{-# LINE 1074 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1074 "src/Test/HMock/Predicates.hs" #-}
      (accept nonNegative 0)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:1077: "
{-# LINE 1077 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1077 "src/Test/HMock/Predicates.hs" #-}
      (accept nonNegative (-1))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1084: "
{-# LINE 1084 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1084 "src/Test/HMock/Predicates.hs" #-}
      (accept finite 1.0)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:1086: "
{-# LINE 1086 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1086 "src/Test/HMock/Predicates.hs" #-}
      (accept finite (0 / 0))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1088: "
{-# LINE 1088 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1088 "src/Test/HMock/Predicates.hs" #-}
      (accept finite (1 / 0))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1106: "
{-# LINE 1106 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1106 "src/Test/HMock/Predicates.hs" #-}
      (accept infinite 1.0)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1108: "
{-# LINE 1108 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1108 "src/Test/HMock/Predicates.hs" #-}
      (accept infinite (0 / 0))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1110: "
{-# LINE 1110 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1110 "src/Test/HMock/Predicates.hs" #-}
      (accept infinite (1 / 0))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:1126: "
{-# LINE 1126 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1126 "src/Test/HMock/Predicates.hs" #-}
      (accept nAn 1.0)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1128: "
{-# LINE 1128 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1128 "src/Test/HMock/Predicates.hs" #-}
      (accept nAn (0 / 0))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:1130: "
{-# LINE 1130 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1130 "src/Test/HMock/Predicates.hs" #-}
      (accept nAn (1 / 0))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1148: "
{-# LINE 1148 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1148 "src/Test/HMock/Predicates.hs" #-}
      (accept (is even) 3)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1150: "
{-# LINE 1150 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1150 "src/Test/HMock/Predicates.hs" #-}
      (accept (is even) 4)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:1168: "
{-# LINE 1168 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1168 "src/Test/HMock/Predicates.hs" #-}
      (accept $(qIs [| even |]) 3)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1170: "
{-# LINE 1170 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1170 "src/Test/HMock/Predicates.hs" #-}
      (accept $(qIs [| even |]) 4)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:1173: "
{-# LINE 1173 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1173 "src/Test/HMock/Predicates.hs" #-}
      (show $(qIs [| even |]))
  [ExpectedLine [LineChunk "\"even\""]]
 DocTest.printPrefix "Test.HMock.Predicates:1191: "
{-# LINE 1191 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1191 "src/Test/HMock/Predicates.hs" #-}
      (accept (with abs (gt 5)) (-6))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:1193: "
{-# LINE 1193 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1193 "src/Test/HMock/Predicates.hs" #-}
      (accept (with abs (gt 5)) (-5))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1195: "
{-# LINE 1195 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1195 "src/Test/HMock/Predicates.hs" #-}
      (accept (with reverse (eq "olleh")) "hello")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:1197: "
{-# LINE 1197 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1197 "src/Test/HMock/Predicates.hs" #-}
      (accept (with reverse (eq "olleh")) "goodbye")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1214: "
{-# LINE 1214 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1214 "src/Test/HMock/Predicates.hs" #-}
      (accept ($(qWith [| abs |]) (gt 5)) (-6))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:1216: "
{-# LINE 1216 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1216 "src/Test/HMock/Predicates.hs" #-}
      (accept ($(qWith [| abs |]) (gt 5)) (-5))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1218: "
{-# LINE 1218 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1218 "src/Test/HMock/Predicates.hs" #-}
      (accept ($(qWith [| reverse |]) (eq "olleh")) "hello")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:1220: "
{-# LINE 1220 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1220 "src/Test/HMock/Predicates.hs" #-}
      (accept ($(qWith [| reverse |]) (eq "olleh")) "goodbye")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1223: "
{-# LINE 1223 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1223 "src/Test/HMock/Predicates.hs" #-}
      (show ($(qWith [| abs |]) (gt 5)))
  [ExpectedLine [LineChunk "\"abs: > 5\""]]
 DocTest.printPrefix "Test.HMock.Predicates:1242: "
{-# LINE 1242 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1242 "src/Test/HMock/Predicates.hs" #-}
      (accept $(qMatch [p| Just (Left _) |]) Nothing)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1244: "
{-# LINE 1244 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1244 "src/Test/HMock/Predicates.hs" #-}
      (accept $(qMatch [p| Just (Left _) |]) (Just (Left 5)))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:1246: "
{-# LINE 1246 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1246 "src/Test/HMock/Predicates.hs" #-}
      (accept $(qMatch [p| Just (Left _) |]) (Just (Right 5)))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1249: "
{-# LINE 1249 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1249 "src/Test/HMock/Predicates.hs" #-}
      (show $(qMatch [p| Just (Left _) |]))
  [ExpectedLine [LineChunk "\"Just (Left _)\""]]
 DocTest.printPrefix "Test.HMock.Predicates:1271: "
{-# LINE 1271 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1271 "src/Test/HMock/Predicates.hs" #-}
      (accept (typed @String anything) "foo")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:1273: "
{-# LINE 1273 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1273 "src/Test/HMock/Predicates.hs" #-}
      (accept (typed @String (sizeIs (gt 5))) "foo")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1275: "
{-# LINE 1275 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1275 "src/Test/HMock/Predicates.hs" #-}
      (accept (typed @String anything) (42 :: Int))
  [ExpectedLine [LineChunk "False"]]
