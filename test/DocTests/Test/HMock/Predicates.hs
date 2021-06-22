-- Do not edit! Automatically created with doctest-extract from src/Test/HMock/Predicates.hs
{-# LINE 78 "src/Test/HMock/Predicates.hs" #-}

{-# OPTIONS_GHC -XTemplateHaskell #-}
{-# OPTIONS_GHC -XTypeApplications #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module DocTests.Test.HMock.Predicates where

import Test.HMock.Predicates
import Test.DocTest.Base
import qualified Test.DocTest.Driver as DocTest

{-# LINE 82 "src/Test/HMock/Predicates.hs" #-}

test :: DocTest.T ()
test = do
 DocTest.printPrefix "Test.HMock.Predicates:98: "
{-# LINE 98 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 98 "src/Test/HMock/Predicates.hs" #-}
      (accept anything "foo")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:100: "
{-# LINE 100 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 100 "src/Test/HMock/Predicates.hs" #-}
      (accept anything undefined)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:111: "
{-# LINE 111 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 111 "src/Test/HMock/Predicates.hs" #-}
      (accept (eq "foo") "foo")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:113: "
{-# LINE 113 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 113 "src/Test/HMock/Predicates.hs" #-}
      (accept (eq "foo") "bar")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:124: "
{-# LINE 124 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 124 "src/Test/HMock/Predicates.hs" #-}
      (accept (neq "foo") "foo")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:126: "
{-# LINE 126 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 126 "src/Test/HMock/Predicates.hs" #-}
      (accept (neq "foo") "bar")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:137: "
{-# LINE 137 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 137 "src/Test/HMock/Predicates.hs" #-}
      (accept (gt 5) 4)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:139: "
{-# LINE 139 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 139 "src/Test/HMock/Predicates.hs" #-}
      (accept (gt 5) 5)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:141: "
{-# LINE 141 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 141 "src/Test/HMock/Predicates.hs" #-}
      (accept (gt 5) 6)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:153: "
{-# LINE 153 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 153 "src/Test/HMock/Predicates.hs" #-}
      (accept (geq 5) 4)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:155: "
{-# LINE 155 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 155 "src/Test/HMock/Predicates.hs" #-}
      (accept (geq 5) 5)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:157: "
{-# LINE 157 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 157 "src/Test/HMock/Predicates.hs" #-}
      (accept (geq 5) 6)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:168: "
{-# LINE 168 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 168 "src/Test/HMock/Predicates.hs" #-}
      (accept (lt 5) 4)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:170: "
{-# LINE 170 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 170 "src/Test/HMock/Predicates.hs" #-}
      (accept (lt 5) 5)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:172: "
{-# LINE 172 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 172 "src/Test/HMock/Predicates.hs" #-}
      (accept (lt 5) 6)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:183: "
{-# LINE 183 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 183 "src/Test/HMock/Predicates.hs" #-}
      (accept (leq 5) 4)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:185: "
{-# LINE 185 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 185 "src/Test/HMock/Predicates.hs" #-}
      (accept (leq 5) 5)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:187: "
{-# LINE 187 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 187 "src/Test/HMock/Predicates.hs" #-}
      (accept (leq 5) 6)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:199: "
{-# LINE 199 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 199 "src/Test/HMock/Predicates.hs" #-}
      (accept (just (eq "value")) Nothing)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:201: "
{-# LINE 201 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 201 "src/Test/HMock/Predicates.hs" #-}
      (accept (just (eq "value")) (Just "value"))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:203: "
{-# LINE 203 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 203 "src/Test/HMock/Predicates.hs" #-}
      (accept (just (eq "value")) (Just "wrong value"))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:215: "
{-# LINE 215 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 215 "src/Test/HMock/Predicates.hs" #-}
      (accept (left (eq "value")) (Left "value"))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:217: "
{-# LINE 217 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 217 "src/Test/HMock/Predicates.hs" #-}
      (accept (left (eq "value")) (Right "value"))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:219: "
{-# LINE 219 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 219 "src/Test/HMock/Predicates.hs" #-}
      (accept (left (eq "value")) (Left "wrong value"))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:231: "
{-# LINE 231 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 231 "src/Test/HMock/Predicates.hs" #-}
      (accept (right (eq "value")) (Right "value"))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:233: "
{-# LINE 233 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 233 "src/Test/HMock/Predicates.hs" #-}
      (accept (right (eq "value")) (Right "wrong value"))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:235: "
{-# LINE 235 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 235 "src/Test/HMock/Predicates.hs" #-}
      (accept (right (eq "value")) (Left "value"))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:247: "
{-# LINE 247 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 247 "src/Test/HMock/Predicates.hs" #-}
      (accept (zipP (eq "foo") (eq "bar")) ("foo", "bar"))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:249: "
{-# LINE 249 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 249 "src/Test/HMock/Predicates.hs" #-}
      (accept (zipP (eq "foo") (eq "bar")) ("bar", "foo"))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:261: "
{-# LINE 261 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 261 "src/Test/HMock/Predicates.hs" #-}
      (accept (zip3P (eq "foo") (eq "bar") (eq "qux")) ("foo", "bar", "qux"))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:263: "
{-# LINE 263 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 263 "src/Test/HMock/Predicates.hs" #-}
      (accept (zip3P (eq "foo") (eq "bar") (eq "qux")) ("qux", "bar", "foo"))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:275: "
{-# LINE 275 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 275 "src/Test/HMock/Predicates.hs" #-}
      (accept (zip4P (eq 1) (eq 2) (eq 3) (eq 4)) (1, 2, 3, 4))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:277: "
{-# LINE 277 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 277 "src/Test/HMock/Predicates.hs" #-}
      (accept (zip4P (eq 1) (eq 2) (eq 3) (eq 4)) (4, 3, 2, 1))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:295: "
{-# LINE 295 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 295 "src/Test/HMock/Predicates.hs" #-}
      (accept (zip5P (eq 1) (eq 2) (eq 3) (eq 4) (eq 5)) (1, 2, 3, 4, 5))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:297: "
{-# LINE 297 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 297 "src/Test/HMock/Predicates.hs" #-}
      (accept (zip5P (eq 1) (eq 2) (eq 3) (eq 4) (eq 5)) (5, 4, 3, 2, 1))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:316: "
{-# LINE 316 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 316 "src/Test/HMock/Predicates.hs" #-}
      (accept (lt "foo" `andP` gt "bar") "eta")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:318: "
{-# LINE 318 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 318 "src/Test/HMock/Predicates.hs" #-}
      (accept (lt "foo" `andP` gt "bar") "quz")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:320: "
{-# LINE 320 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 320 "src/Test/HMock/Predicates.hs" #-}
      (accept (lt "foo" `andP` gt "bar") "alpha")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:331: "
{-# LINE 331 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 331 "src/Test/HMock/Predicates.hs" #-}
      (accept (lt "bar" `orP` gt "foo") "eta")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:333: "
{-# LINE 333 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 333 "src/Test/HMock/Predicates.hs" #-}
      (accept (lt "bar" `orP` gt "foo") "quz")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:335: "
{-# LINE 335 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 335 "src/Test/HMock/Predicates.hs" #-}
      (accept (lt "bar" `orP` gt "foo") "alpha")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:347: "
{-# LINE 347 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 347 "src/Test/HMock/Predicates.hs" #-}
      (accept (notP (eq "negative")) "positive")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:349: "
{-# LINE 349 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 349 "src/Test/HMock/Predicates.hs" #-}
      (accept (notP (eq "negative")) "negative")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:360: "
{-# LINE 360 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 360 "src/Test/HMock/Predicates.hs" #-}
      (accept (startsWith "fun") "fungible")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:362: "
{-# LINE 362 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 362 "src/Test/HMock/Predicates.hs" #-}
      (accept (startsWith "gib") "fungible")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:373: "
{-# LINE 373 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 373 "src/Test/HMock/Predicates.hs" #-}
      (accept (endsWith "ow") "crossbow")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:375: "
{-# LINE 375 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 375 "src/Test/HMock/Predicates.hs" #-}
      (accept (endsWith "ow") "trebuchet")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:387: "
{-# LINE 387 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 387 "src/Test/HMock/Predicates.hs" #-}
      (accept (hasSubstr "i") "team")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:389: "
{-# LINE 389 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 389 "src/Test/HMock/Predicates.hs" #-}
      (accept (hasSubstr "i") "partnership")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:401: "
{-# LINE 401 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 401 "src/Test/HMock/Predicates.hs" #-}
      (accept (hasSubsequence [1..5]) [1, 2, 3, 4, 5])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:403: "
{-# LINE 403 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 403 "src/Test/HMock/Predicates.hs" #-}
      (accept (hasSubsequence [1..5]) [0, 1, 0, 2, 0, 3, 0, 4, 0, 5, 0])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:405: "
{-# LINE 405 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 405 "src/Test/HMock/Predicates.hs" #-}
      (accept (hasSubsequence [1..5]) [2, 3, 5, 7, 11])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:417: "
{-# LINE 417 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 417 "src/Test/HMock/Predicates.hs" #-}
      (accept (caseInsensitive startsWith "foo") "FOOTBALL!")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:419: "
{-# LINE 419 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 419 "src/Test/HMock/Predicates.hs" #-}
      (accept (caseInsensitive endsWith "ball") "soccer")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:421: "
{-# LINE 421 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 421 "src/Test/HMock/Predicates.hs" #-}
      (accept (caseInsensitive eq "time") "TIME")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:423: "
{-# LINE 423 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 423 "src/Test/HMock/Predicates.hs" #-}
      (accept (caseInsensitive gt "NOTHING") "everything")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:448: "
{-# LINE 448 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 448 "src/Test/HMock/Predicates.hs" #-}
      (accept (matchesRegex "x{2,5}y?") "xxxy")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:450: "
{-# LINE 450 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 450 "src/Test/HMock/Predicates.hs" #-}
      (accept (matchesRegex "x{2,5}y?") "xyy")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:452: "
{-# LINE 452 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 452 "src/Test/HMock/Predicates.hs" #-}
      (accept (matchesRegex "x{2,5}y?") "wxxxyz")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:475: "
{-# LINE 475 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 475 "src/Test/HMock/Predicates.hs" #-}
      (accept (matchesCaseInsensitiveRegex "x{2,5}y?") "XXXY")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:477: "
{-# LINE 477 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 477 "src/Test/HMock/Predicates.hs" #-}
      (accept (matchesCaseInsensitiveRegex "x{2,5}y?") "XYY")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:479: "
{-# LINE 479 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 479 "src/Test/HMock/Predicates.hs" #-}
      (accept (matchesCaseInsensitiveRegex "x{2,5}y?") "WXXXYZ")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:508: "
{-# LINE 508 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 508 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsRegex "x{2,5}y?") "xxxy")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:510: "
{-# LINE 510 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 510 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsRegex "x{2,5}y?") "xyy")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:512: "
{-# LINE 512 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 512 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsRegex "x{2,5}y?") "wxxxyz")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:533: "
{-# LINE 533 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 533 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsCaseInsensitiveRegex "x{2,5}y?") "XXXY")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:535: "
{-# LINE 535 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 535 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsCaseInsensitiveRegex "x{2,5}y?") "XYY")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:537: "
{-# LINE 537 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 537 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsCaseInsensitiveRegex "x{2,5}y?") "WXXXYZ")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:558: "
{-# LINE 558 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 558 "src/Test/HMock/Predicates.hs" #-}
      (accept isEmpty [])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:560: "
{-# LINE 560 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 560 "src/Test/HMock/Predicates.hs" #-}
      (accept isEmpty [1, 2, 3])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:562: "
{-# LINE 562 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 562 "src/Test/HMock/Predicates.hs" #-}
      (accept isEmpty "")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:564: "
{-# LINE 564 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 564 "src/Test/HMock/Predicates.hs" #-}
      (accept isEmpty "gas tank")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:575: "
{-# LINE 575 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 575 "src/Test/HMock/Predicates.hs" #-}
      (accept nonEmpty [])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:577: "
{-# LINE 577 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 577 "src/Test/HMock/Predicates.hs" #-}
      (accept nonEmpty [1, 2, 3])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:579: "
{-# LINE 579 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 579 "src/Test/HMock/Predicates.hs" #-}
      (accept nonEmpty "")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:581: "
{-# LINE 581 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 581 "src/Test/HMock/Predicates.hs" #-}
      (accept nonEmpty "gas tank")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:593: "
{-# LINE 593 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 593 "src/Test/HMock/Predicates.hs" #-}
      (accept (sizeIs (lt 3)) ['a' .. 'f'])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:595: "
{-# LINE 595 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 595 "src/Test/HMock/Predicates.hs" #-}
      (accept (sizeIs (lt 3)) ['a' .. 'b'])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:607: "
{-# LINE 607 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 607 "src/Test/HMock/Predicates.hs" #-}
      (accept (elemsAre [lt 3, lt 4, lt 5]) [2, 3, 4])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:609: "
{-# LINE 609 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 609 "src/Test/HMock/Predicates.hs" #-}
      (accept (elemsAre [lt 3, lt 4, lt 5]) [2, 3, 4, 5])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:611: "
{-# LINE 611 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 611 "src/Test/HMock/Predicates.hs" #-}
      (accept (elemsAre [lt 3, lt 4, lt 5]) [2, 10, 4])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:625: "
{-# LINE 625 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 625 "src/Test/HMock/Predicates.hs" #-}
      (accept (unorderedElemsAre [eq 1, eq 2, eq 3]) [1, 2, 3])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:627: "
{-# LINE 627 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 627 "src/Test/HMock/Predicates.hs" #-}
      (accept (unorderedElemsAre [eq 1, eq 2, eq 3]) [2, 3, 1])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:629: "
{-# LINE 629 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 629 "src/Test/HMock/Predicates.hs" #-}
      (accept (unorderedElemsAre [eq 1, eq 2, eq 3]) [1, 2, 3, 4])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:631: "
{-# LINE 631 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 631 "src/Test/HMock/Predicates.hs" #-}
      (accept (unorderedElemsAre [eq 1, eq 2, eq 3]) [1, 3])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:647: "
{-# LINE 647 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 647 "src/Test/HMock/Predicates.hs" #-}
      (accept (each (gt 5)) [4, 5, 6])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:649: "
{-# LINE 649 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 649 "src/Test/HMock/Predicates.hs" #-}
      (accept (each (gt 5)) [6, 7, 8])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:651: "
{-# LINE 651 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 651 "src/Test/HMock/Predicates.hs" #-}
      (accept (each (gt 5)) [])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:663: "
{-# LINE 663 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 663 "src/Test/HMock/Predicates.hs" #-}
      (accept (contains (gt 5)) [3, 4, 5])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:665: "
{-# LINE 665 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 665 "src/Test/HMock/Predicates.hs" #-}
      (accept (contains (gt 5)) [4, 5, 6])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:667: "
{-# LINE 667 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 667 "src/Test/HMock/Predicates.hs" #-}
      (accept (contains (gt 5)) [])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:681: "
{-# LINE 681 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 681 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsAll [eq "foo", eq "bar"]) ["bar", "foo"])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:683: "
{-# LINE 683 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 683 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsAll [eq "foo", eq "bar"]) ["foo"])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:685: "
{-# LINE 685 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 685 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsAll [eq "foo", eq "bar"]) ["foo", "bar", "qux"])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:698: "
{-# LINE 698 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 698 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsOnly [eq "foo", eq "bar"]) ["foo", "foo"])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:700: "
{-# LINE 700 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 700 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsOnly [eq "foo", eq "bar"]) ["foo", "bar"])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:702: "
{-# LINE 702 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 702 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsOnly [eq "foo", eq "bar"]) ["foo", "qux"])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:714: "
{-# LINE 714 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 714 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsKey (eq "foo")) [("foo", 1), ("bar", 2)])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:716: "
{-# LINE 716 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 716 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsKey (eq "foo")) [("bar", 1), ("qux", 2)])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:729: "
{-# LINE 729 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 729 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsEntry (eq "foo") (gt 10)) [("foo", 12), ("bar", 5)])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:731: "
{-# LINE 731 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 731 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsEntry (eq "foo") (gt 10)) [("foo", 5), ("bar", 12)])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:733: "
{-# LINE 733 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 733 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsEntry (eq "foo") (gt 10)) [("bar", 12)])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:746: "
{-# LINE 746 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 746 "src/Test/HMock/Predicates.hs" #-}
      (accept (keysAre [eq "a", eq "b", eq "c"]) [("a", 1), ("b", 2), ("c", 3)])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:748: "
{-# LINE 748 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 748 "src/Test/HMock/Predicates.hs" #-}
      (accept (keysAre [eq "a", eq "b", eq "c"]) [("c", 1), ("b", 2), ("a", 3)])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:750: "
{-# LINE 750 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 750 "src/Test/HMock/Predicates.hs" #-}
      (accept (keysAre [eq "a", eq "b", eq "c"]) [("a", 1), ("c", 3)])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:752: "
{-# LINE 752 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 752 "src/Test/HMock/Predicates.hs" #-}
      (accept (keysAre [eq "a", eq "b"]) [("a", 1), ("b", 2), ("c", 3)])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:768: "
{-# LINE 768 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 768 "src/Test/HMock/Predicates.hs" #-}
      (accept (entriesAre [(eq 1, eq 2), (eq 3, eq 4)]) [(1, 2), (3, 4)])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:770: "
{-# LINE 770 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 770 "src/Test/HMock/Predicates.hs" #-}
      (accept (entriesAre [(eq 1, eq 2), (eq 3, eq 4)]) [(3, 4), (1, 2)])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:772: "
{-# LINE 772 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 772 "src/Test/HMock/Predicates.hs" #-}
      (accept (entriesAre [(eq 1, eq 2), (eq 3, eq 4)]) [(1, 4), (3, 2)])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:774: "
{-# LINE 774 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 774 "src/Test/HMock/Predicates.hs" #-}
      (accept (entriesAre [(eq 1, eq 2), (eq 3, eq 4)]) [(1, 2), (3, 4), (5, 6)])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:795: "
{-# LINE 795 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 795 "src/Test/HMock/Predicates.hs" #-}
      (accept (eq 1.0) (sum (replicate 100 0.01)))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:802: "
{-# LINE 802 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 802 "src/Test/HMock/Predicates.hs" #-}
      (accept (approxEq 1.0) (sum (replicate 100 0.01)))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:804: "
{-# LINE 804 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 804 "src/Test/HMock/Predicates.hs" #-}
      (accept (approxEq 1.0) (sum (replicate 100 0.009999)))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:817: "
{-# LINE 817 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 817 "src/Test/HMock/Predicates.hs" #-}
      (accept finite 1.0)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:819: "
{-# LINE 819 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 819 "src/Test/HMock/Predicates.hs" #-}
      (accept finite (0 / 0))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:821: "
{-# LINE 821 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 821 "src/Test/HMock/Predicates.hs" #-}
      (accept finite (1 / 0))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:832: "
{-# LINE 832 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 832 "src/Test/HMock/Predicates.hs" #-}
      (accept infinite 1.0)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:834: "
{-# LINE 834 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 834 "src/Test/HMock/Predicates.hs" #-}
      (accept infinite (0 / 0))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:836: "
{-# LINE 836 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 836 "src/Test/HMock/Predicates.hs" #-}
      (accept infinite (1 / 0))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:847: "
{-# LINE 847 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 847 "src/Test/HMock/Predicates.hs" #-}
      (accept nAn 1.0)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:849: "
{-# LINE 849 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 849 "src/Test/HMock/Predicates.hs" #-}
      (accept nAn (0 / 0))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:851: "
{-# LINE 851 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 851 "src/Test/HMock/Predicates.hs" #-}
      (accept nAn (1 / 0))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:864: "
{-# LINE 864 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 864 "src/Test/HMock/Predicates.hs" #-}
      (accept (is even) 3)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:866: "
{-# LINE 866 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 866 "src/Test/HMock/Predicates.hs" #-}
      (accept (is even) 4)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:879: "
{-# LINE 879 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 879 "src/Test/HMock/Predicates.hs" #-}
      (accept $(qIs [| even |]) 3)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:881: "
{-# LINE 881 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 881 "src/Test/HMock/Predicates.hs" #-}
      (accept $(qIs [| even |]) 4)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:884: "
{-# LINE 884 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 884 "src/Test/HMock/Predicates.hs" #-}
      (show $(qIs [| even |]))
  [ExpectedLine [LineChunk "\"even\""]]
 DocTest.printPrefix "Test.HMock.Predicates:898: "
{-# LINE 898 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 898 "src/Test/HMock/Predicates.hs" #-}
      (accept (with abs (gt 5)) (-6))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:900: "
{-# LINE 900 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 900 "src/Test/HMock/Predicates.hs" #-}
      (accept (with abs (gt 5)) (-5))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:902: "
{-# LINE 902 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 902 "src/Test/HMock/Predicates.hs" #-}
      (accept (with reverse (eq "olleh")) "hello")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:904: "
{-# LINE 904 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 904 "src/Test/HMock/Predicates.hs" #-}
      (accept (with reverse (eq "olleh")) "goodbye")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:918: "
{-# LINE 918 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 918 "src/Test/HMock/Predicates.hs" #-}
      (accept ($(qWith [| abs |]) (gt 5)) (-6))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:920: "
{-# LINE 920 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 920 "src/Test/HMock/Predicates.hs" #-}
      (accept ($(qWith [| abs |]) (gt 5)) (-5))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:922: "
{-# LINE 922 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 922 "src/Test/HMock/Predicates.hs" #-}
      (accept ($(qWith [| reverse |]) (eq "olleh")) "hello")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:924: "
{-# LINE 924 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 924 "src/Test/HMock/Predicates.hs" #-}
      (accept ($(qWith [| reverse |]) (eq "olleh")) "goodbye")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:927: "
{-# LINE 927 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 927 "src/Test/HMock/Predicates.hs" #-}
      (show ($(qWith [| abs |]) (gt 5)))
  [ExpectedLine [LineChunk "\"abs: > 5\""]]
 DocTest.printPrefix "Test.HMock.Predicates:943: "
{-# LINE 943 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 943 "src/Test/HMock/Predicates.hs" #-}
      (accept $(qMatch [p| Just (Left _) |]) Nothing)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:945: "
{-# LINE 945 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 945 "src/Test/HMock/Predicates.hs" #-}
      (accept $(qMatch [p| Just (Left _) |]) (Just (Left 5)))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:947: "
{-# LINE 947 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 947 "src/Test/HMock/Predicates.hs" #-}
      (accept $(qMatch [p| Just (Left _) |]) (Just (Right 5)))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:950: "
{-# LINE 950 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 950 "src/Test/HMock/Predicates.hs" #-}
      (show $(qMatch [p| Just (Left _) |]))
  [ExpectedLine [LineChunk "\"Just (Left _)\""]]
 DocTest.printPrefix "Test.HMock.Predicates:966: "
{-# LINE 966 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 966 "src/Test/HMock/Predicates.hs" #-}
      (accept (typed @String anything) "foo")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:968: "
{-# LINE 968 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 968 "src/Test/HMock/Predicates.hs" #-}
      (accept (typed @String (sizeIs (gt 5))) "foo")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:970: "
{-# LINE 970 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 970 "src/Test/HMock/Predicates.hs" #-}
      (accept (typed @String anything) (42 :: Int))
  [ExpectedLine [LineChunk "False"]]
