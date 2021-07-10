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
 DocTest.printPrefix "Test.HMock.Predicates:103: "
{-# LINE 103 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 103 "src/Test/HMock/Predicates.hs" #-}
      (accept anything "foo")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:105: "
{-# LINE 105 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 105 "src/Test/HMock/Predicates.hs" #-}
      (accept anything undefined)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:118: "
{-# LINE 118 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 118 "src/Test/HMock/Predicates.hs" #-}
      (accept (eq "foo") "foo")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:120: "
{-# LINE 120 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 120 "src/Test/HMock/Predicates.hs" #-}
      (accept (eq "foo") "bar")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:137: "
{-# LINE 137 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 137 "src/Test/HMock/Predicates.hs" #-}
      (accept (neq "foo") "foo")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:139: "
{-# LINE 139 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 139 "src/Test/HMock/Predicates.hs" #-}
      (accept (neq "foo") "bar")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:146: "
{-# LINE 146 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 146 "src/Test/HMock/Predicates.hs" #-}
      (accept (gt 5) 4)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:148: "
{-# LINE 148 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 148 "src/Test/HMock/Predicates.hs" #-}
      (accept (gt 5) 5)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:150: "
{-# LINE 150 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 150 "src/Test/HMock/Predicates.hs" #-}
      (accept (gt 5) 6)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:164: "
{-# LINE 164 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 164 "src/Test/HMock/Predicates.hs" #-}
      (accept (geq 5) 4)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:166: "
{-# LINE 166 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 166 "src/Test/HMock/Predicates.hs" #-}
      (accept (geq 5) 5)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:168: "
{-# LINE 168 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 168 "src/Test/HMock/Predicates.hs" #-}
      (accept (geq 5) 6)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:181: "
{-# LINE 181 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 181 "src/Test/HMock/Predicates.hs" #-}
      (accept (lt 5) 4)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:183: "
{-# LINE 183 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 183 "src/Test/HMock/Predicates.hs" #-}
      (accept (lt 5) 5)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:185: "
{-# LINE 185 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 185 "src/Test/HMock/Predicates.hs" #-}
      (accept (lt 5) 6)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:192: "
{-# LINE 192 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 192 "src/Test/HMock/Predicates.hs" #-}
      (accept (leq 5) 4)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:194: "
{-# LINE 194 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 194 "src/Test/HMock/Predicates.hs" #-}
      (accept (leq 5) 5)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:196: "
{-# LINE 196 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 196 "src/Test/HMock/Predicates.hs" #-}
      (accept (leq 5) 6)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:204: "
{-# LINE 204 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 204 "src/Test/HMock/Predicates.hs" #-}
      (accept (just (eq "value")) Nothing)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:206: "
{-# LINE 206 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 206 "src/Test/HMock/Predicates.hs" #-}
      (accept (just (eq "value")) (Just "value"))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:208: "
{-# LINE 208 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 208 "src/Test/HMock/Predicates.hs" #-}
      (accept (just (eq "value")) (Just "wrong value"))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:231: "
{-# LINE 231 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 231 "src/Test/HMock/Predicates.hs" #-}
      (accept (left (eq "value")) (Left "value"))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:233: "
{-# LINE 233 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 233 "src/Test/HMock/Predicates.hs" #-}
      (accept (left (eq "value")) (Right "value"))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:235: "
{-# LINE 235 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 235 "src/Test/HMock/Predicates.hs" #-}
      (accept (left (eq "value")) (Left "wrong value"))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:249: "
{-# LINE 249 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 249 "src/Test/HMock/Predicates.hs" #-}
      (accept (right (eq "value")) (Right "value"))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:251: "
{-# LINE 251 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 251 "src/Test/HMock/Predicates.hs" #-}
      (accept (right (eq "value")) (Right "wrong value"))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:253: "
{-# LINE 253 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 253 "src/Test/HMock/Predicates.hs" #-}
      (accept (right (eq "value")) (Left "value"))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:267: "
{-# LINE 267 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 267 "src/Test/HMock/Predicates.hs" #-}
      (accept (zipP (eq "foo") (eq "bar")) ("foo", "bar"))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:269: "
{-# LINE 269 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 269 "src/Test/HMock/Predicates.hs" #-}
      (accept (zipP (eq "foo") (eq "bar")) ("bar", "foo"))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:283: "
{-# LINE 283 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 283 "src/Test/HMock/Predicates.hs" #-}
      (accept (zip3P (eq "foo") (eq "bar") (eq "qux")) ("foo", "bar", "qux"))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:285: "
{-# LINE 285 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 285 "src/Test/HMock/Predicates.hs" #-}
      (accept (zip3P (eq "foo") (eq "bar") (eq "qux")) ("qux", "bar", "foo"))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:299: "
{-# LINE 299 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 299 "src/Test/HMock/Predicates.hs" #-}
      (accept (zip4P (eq 1) (eq 2) (eq 3) (eq 4)) (1, 2, 3, 4))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:301: "
{-# LINE 301 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 301 "src/Test/HMock/Predicates.hs" #-}
      (accept (zip4P (eq 1) (eq 2) (eq 3) (eq 4)) (4, 3, 2, 1))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:321: "
{-# LINE 321 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 321 "src/Test/HMock/Predicates.hs" #-}
      (accept (zip5P (eq 1) (eq 2) (eq 3) (eq 4) (eq 5)) (1, 2, 3, 4, 5))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:323: "
{-# LINE 323 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 323 "src/Test/HMock/Predicates.hs" #-}
      (accept (zip5P (eq 1) (eq 2) (eq 3) (eq 4) (eq 5)) (5, 4, 3, 2, 1))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:344: "
{-# LINE 344 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 344 "src/Test/HMock/Predicates.hs" #-}
      (accept (lt "foo" `andP` gt "bar") "eta")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:346: "
{-# LINE 346 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 346 "src/Test/HMock/Predicates.hs" #-}
      (accept (lt "foo" `andP` gt "bar") "quz")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:348: "
{-# LINE 348 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 348 "src/Test/HMock/Predicates.hs" #-}
      (accept (lt "foo" `andP` gt "bar") "alpha")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:361: "
{-# LINE 361 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 361 "src/Test/HMock/Predicates.hs" #-}
      (accept (lt "bar" `orP` gt "foo") "eta")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:363: "
{-# LINE 363 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 363 "src/Test/HMock/Predicates.hs" #-}
      (accept (lt "bar" `orP` gt "foo") "quz")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:365: "
{-# LINE 365 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 365 "src/Test/HMock/Predicates.hs" #-}
      (accept (lt "bar" `orP` gt "foo") "alpha")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:373: "
{-# LINE 373 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 373 "src/Test/HMock/Predicates.hs" #-}
      (accept (notP (eq "negative")) "positive")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:375: "
{-# LINE 375 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 375 "src/Test/HMock/Predicates.hs" #-}
      (accept (notP (eq "negative")) "negative")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:388: "
{-# LINE 388 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 388 "src/Test/HMock/Predicates.hs" #-}
      (accept (startsWith "fun") "fungible")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:390: "
{-# LINE 390 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 390 "src/Test/HMock/Predicates.hs" #-}
      (accept (startsWith "gib") "fungible")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:403: "
{-# LINE 403 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 403 "src/Test/HMock/Predicates.hs" #-}
      (accept (endsWith "ow") "crossbow")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:405: "
{-# LINE 405 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 405 "src/Test/HMock/Predicates.hs" #-}
      (accept (endsWith "ow") "trebuchet")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:419: "
{-# LINE 419 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 419 "src/Test/HMock/Predicates.hs" #-}
      (accept (hasSubstr "i") "team")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:421: "
{-# LINE 421 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 421 "src/Test/HMock/Predicates.hs" #-}
      (accept (hasSubstr "i") "partnership")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:435: "
{-# LINE 435 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 435 "src/Test/HMock/Predicates.hs" #-}
      (accept (hasSubsequence [1..5]) [1, 2, 3, 4, 5])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:437: "
{-# LINE 437 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 437 "src/Test/HMock/Predicates.hs" #-}
      (accept (hasSubsequence [1..5]) [0, 1, 0, 2, 0, 3, 0, 4, 0, 5, 0])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:439: "
{-# LINE 439 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 439 "src/Test/HMock/Predicates.hs" #-}
      (accept (hasSubsequence [1..5]) [2, 3, 5, 7, 11])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:453: "
{-# LINE 453 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 453 "src/Test/HMock/Predicates.hs" #-}
      (accept (caseInsensitive startsWith "foo") "FOOTBALL!")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:455: "
{-# LINE 455 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 455 "src/Test/HMock/Predicates.hs" #-}
      (accept (caseInsensitive endsWith "ball") "soccer")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:457: "
{-# LINE 457 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 457 "src/Test/HMock/Predicates.hs" #-}
      (accept (caseInsensitive eq "time") "TIME")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:459: "
{-# LINE 459 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 459 "src/Test/HMock/Predicates.hs" #-}
      (accept (caseInsensitive gt "NOTHING") "everything")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:486: "
{-# LINE 486 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 486 "src/Test/HMock/Predicates.hs" #-}
      (accept (matchesRegex "x{2,5}y?") "xxxy")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:488: "
{-# LINE 488 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 488 "src/Test/HMock/Predicates.hs" #-}
      (accept (matchesRegex "x{2,5}y?") "xyy")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:490: "
{-# LINE 490 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 490 "src/Test/HMock/Predicates.hs" #-}
      (accept (matchesRegex "x{2,5}y?") "wxxxyz")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:515: "
{-# LINE 515 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 515 "src/Test/HMock/Predicates.hs" #-}
      (accept (matchesCaseInsensitiveRegex "x{2,5}y?") "XXXY")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:517: "
{-# LINE 517 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 517 "src/Test/HMock/Predicates.hs" #-}
      (accept (matchesCaseInsensitiveRegex "x{2,5}y?") "XYY")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:519: "
{-# LINE 519 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 519 "src/Test/HMock/Predicates.hs" #-}
      (accept (matchesCaseInsensitiveRegex "x{2,5}y?") "WXXXYZ")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:550: "
{-# LINE 550 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 550 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsRegex "x{2,5}y?") "xxxy")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:552: "
{-# LINE 552 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 552 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsRegex "x{2,5}y?") "xyy")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:554: "
{-# LINE 554 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 554 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsRegex "x{2,5}y?") "wxxxyz")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:577: "
{-# LINE 577 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 577 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsCaseInsensitiveRegex "x{2,5}y?") "XXXY")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:579: "
{-# LINE 579 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 579 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsCaseInsensitiveRegex "x{2,5}y?") "XYY")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:581: "
{-# LINE 581 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 581 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsCaseInsensitiveRegex "x{2,5}y?") "WXXXYZ")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:604: "
{-# LINE 604 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 604 "src/Test/HMock/Predicates.hs" #-}
      (accept isEmpty [])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:606: "
{-# LINE 606 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 606 "src/Test/HMock/Predicates.hs" #-}
      (accept isEmpty [1, 2, 3])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:608: "
{-# LINE 608 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 608 "src/Test/HMock/Predicates.hs" #-}
      (accept isEmpty "")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:610: "
{-# LINE 610 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 610 "src/Test/HMock/Predicates.hs" #-}
      (accept isEmpty "gas tank")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:623: "
{-# LINE 623 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 623 "src/Test/HMock/Predicates.hs" #-}
      (accept nonEmpty [])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:625: "
{-# LINE 625 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 625 "src/Test/HMock/Predicates.hs" #-}
      (accept nonEmpty [1, 2, 3])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:627: "
{-# LINE 627 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 627 "src/Test/HMock/Predicates.hs" #-}
      (accept nonEmpty "")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:629: "
{-# LINE 629 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 629 "src/Test/HMock/Predicates.hs" #-}
      (accept nonEmpty "gas tank")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:637: "
{-# LINE 637 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 637 "src/Test/HMock/Predicates.hs" #-}
      (accept (sizeIs (lt 3)) ['a' .. 'f'])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:639: "
{-# LINE 639 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 639 "src/Test/HMock/Predicates.hs" #-}
      (accept (sizeIs (lt 3)) ['a' .. 'b'])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:653: "
{-# LINE 653 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 653 "src/Test/HMock/Predicates.hs" #-}
      (accept (elemsAre [lt 3, lt 4, lt 5]) [2, 3, 4])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:655: "
{-# LINE 655 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 655 "src/Test/HMock/Predicates.hs" #-}
      (accept (elemsAre [lt 3, lt 4, lt 5]) [2, 3, 4, 5])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:657: "
{-# LINE 657 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 657 "src/Test/HMock/Predicates.hs" #-}
      (accept (elemsAre [lt 3, lt 4, lt 5]) [2, 10, 4])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:673: "
{-# LINE 673 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 673 "src/Test/HMock/Predicates.hs" #-}
      (accept (unorderedElemsAre [eq 1, eq 2, eq 3]) [1, 2, 3])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:675: "
{-# LINE 675 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 675 "src/Test/HMock/Predicates.hs" #-}
      (accept (unorderedElemsAre [eq 1, eq 2, eq 3]) [2, 3, 1])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:677: "
{-# LINE 677 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 677 "src/Test/HMock/Predicates.hs" #-}
      (accept (unorderedElemsAre [eq 1, eq 2, eq 3]) [1, 2, 3, 4])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:679: "
{-# LINE 679 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 679 "src/Test/HMock/Predicates.hs" #-}
      (accept (unorderedElemsAre [eq 1, eq 2, eq 3]) [1, 3])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:698: "
{-# LINE 698 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 698 "src/Test/HMock/Predicates.hs" #-}
      (accept (each (gt 5)) [4, 5, 6])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:700: "
{-# LINE 700 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 700 "src/Test/HMock/Predicates.hs" #-}
      (accept (each (gt 5)) [6, 7, 8])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:702: "
{-# LINE 702 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 702 "src/Test/HMock/Predicates.hs" #-}
      (accept (each (gt 5)) [])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:716: "
{-# LINE 716 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 716 "src/Test/HMock/Predicates.hs" #-}
      (accept (contains (gt 5)) [3, 4, 5])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:718: "
{-# LINE 718 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 718 "src/Test/HMock/Predicates.hs" #-}
      (accept (contains (gt 5)) [4, 5, 6])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:720: "
{-# LINE 720 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 720 "src/Test/HMock/Predicates.hs" #-}
      (accept (contains (gt 5)) [])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:730: "
{-# LINE 730 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 730 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsAll [eq "foo", eq "bar"]) ["bar", "foo"])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:732: "
{-# LINE 732 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 732 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsAll [eq "foo", eq "bar"]) ["foo"])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:734: "
{-# LINE 734 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 734 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsAll [eq "foo", eq "bar"]) ["foo", "bar", "qux"])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:749: "
{-# LINE 749 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 749 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsOnly [eq "foo", eq "bar"]) ["foo", "foo"])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:751: "
{-# LINE 751 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 751 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsOnly [eq "foo", eq "bar"]) ["foo", "bar"])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:753: "
{-# LINE 753 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 753 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsOnly [eq "foo", eq "bar"]) ["foo", "qux"])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:767: "
{-# LINE 767 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 767 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsKey (eq "foo")) [("foo", 1), ("bar", 2)])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:769: "
{-# LINE 769 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 769 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsKey (eq "foo")) [("bar", 1), ("qux", 2)])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:784: "
{-# LINE 784 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 784 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsEntry (eq "foo") (gt 10)) [("foo", 12), ("bar", 5)])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:786: "
{-# LINE 786 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 786 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsEntry (eq "foo") (gt 10)) [("foo", 5), ("bar", 12)])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:788: "
{-# LINE 788 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 788 "src/Test/HMock/Predicates.hs" #-}
      (accept (containsEntry (eq "foo") (gt 10)) [("bar", 12)])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:803: "
{-# LINE 803 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 803 "src/Test/HMock/Predicates.hs" #-}
      (accept (keysAre [eq "a", eq "b", eq "c"]) [("a", 1), ("b", 2), ("c", 3)])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:805: "
{-# LINE 805 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 805 "src/Test/HMock/Predicates.hs" #-}
      (accept (keysAre [eq "a", eq "b", eq "c"]) [("c", 1), ("b", 2), ("a", 3)])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:807: "
{-# LINE 807 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 807 "src/Test/HMock/Predicates.hs" #-}
      (accept (keysAre [eq "a", eq "b", eq "c"]) [("a", 1), ("c", 3)])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:809: "
{-# LINE 809 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 809 "src/Test/HMock/Predicates.hs" #-}
      (accept (keysAre [eq "a", eq "b"]) [("a", 1), ("b", 2), ("c", 3)])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:827: "
{-# LINE 827 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 827 "src/Test/HMock/Predicates.hs" #-}
      (accept (entriesAre [(eq 1, eq 2), (eq 3, eq 4)]) [(1, 2), (3, 4)])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:829: "
{-# LINE 829 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 829 "src/Test/HMock/Predicates.hs" #-}
      (accept (entriesAre [(eq 1, eq 2), (eq 3, eq 4)]) [(3, 4), (1, 2)])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:831: "
{-# LINE 831 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 831 "src/Test/HMock/Predicates.hs" #-}
      (accept (entriesAre [(eq 1, eq 2), (eq 3, eq 4)]) [(1, 4), (3, 2)])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:833: "
{-# LINE 833 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 833 "src/Test/HMock/Predicates.hs" #-}
      (accept (entriesAre [(eq 1, eq 2), (eq 3, eq 4)]) [(1, 2), (3, 4), (5, 6)])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:856: "
{-# LINE 856 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 856 "src/Test/HMock/Predicates.hs" #-}
      (accept (eq 1.0) (sum (replicate 100 0.01)))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:863: "
{-# LINE 863 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 863 "src/Test/HMock/Predicates.hs" #-}
      (accept (approxEq 1.0) (sum (replicate 100 0.01)))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:865: "
{-# LINE 865 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 865 "src/Test/HMock/Predicates.hs" #-}
      (accept (approxEq 1.0) (sum (replicate 100 0.009999)))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:880: "
{-# LINE 880 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 880 "src/Test/HMock/Predicates.hs" #-}
      (accept finite 1.0)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:882: "
{-# LINE 882 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 882 "src/Test/HMock/Predicates.hs" #-}
      (accept finite (0 / 0))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:884: "
{-# LINE 884 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 884 "src/Test/HMock/Predicates.hs" #-}
      (accept finite (1 / 0))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:897: "
{-# LINE 897 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 897 "src/Test/HMock/Predicates.hs" #-}
      (accept infinite 1.0)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:899: "
{-# LINE 899 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 899 "src/Test/HMock/Predicates.hs" #-}
      (accept infinite (0 / 0))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:901: "
{-# LINE 901 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 901 "src/Test/HMock/Predicates.hs" #-}
      (accept infinite (1 / 0))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:914: "
{-# LINE 914 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 914 "src/Test/HMock/Predicates.hs" #-}
      (accept nAn 1.0)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:916: "
{-# LINE 916 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 916 "src/Test/HMock/Predicates.hs" #-}
      (accept nAn (0 / 0))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:918: "
{-# LINE 918 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 918 "src/Test/HMock/Predicates.hs" #-}
      (accept nAn (1 / 0))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:933: "
{-# LINE 933 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 933 "src/Test/HMock/Predicates.hs" #-}
      (accept (is even) 3)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:935: "
{-# LINE 935 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 935 "src/Test/HMock/Predicates.hs" #-}
      (accept (is even) 4)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:950: "
{-# LINE 950 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 950 "src/Test/HMock/Predicates.hs" #-}
      (accept $(qIs [| even |]) 3)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:952: "
{-# LINE 952 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 952 "src/Test/HMock/Predicates.hs" #-}
      (accept $(qIs [| even |]) 4)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:955: "
{-# LINE 955 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 955 "src/Test/HMock/Predicates.hs" #-}
      (show $(qIs [| even |]))
  [ExpectedLine [LineChunk "\"even\""]]
 DocTest.printPrefix "Test.HMock.Predicates:971: "
{-# LINE 971 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 971 "src/Test/HMock/Predicates.hs" #-}
      (accept (with abs (gt 5)) (-6))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:973: "
{-# LINE 973 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 973 "src/Test/HMock/Predicates.hs" #-}
      (accept (with abs (gt 5)) (-5))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:975: "
{-# LINE 975 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 975 "src/Test/HMock/Predicates.hs" #-}
      (accept (with reverse (eq "olleh")) "hello")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:977: "
{-# LINE 977 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 977 "src/Test/HMock/Predicates.hs" #-}
      (accept (with reverse (eq "olleh")) "goodbye")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:994: "
{-# LINE 994 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 994 "src/Test/HMock/Predicates.hs" #-}
      (accept ($(qWith [| abs |]) (gt 5)) (-6))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:996: "
{-# LINE 996 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 996 "src/Test/HMock/Predicates.hs" #-}
      (accept ($(qWith [| abs |]) (gt 5)) (-5))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:998: "
{-# LINE 998 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 998 "src/Test/HMock/Predicates.hs" #-}
      (accept ($(qWith [| reverse |]) (eq "olleh")) "hello")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:1000: "
{-# LINE 1000 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1000 "src/Test/HMock/Predicates.hs" #-}
      (accept ($(qWith [| reverse |]) (eq "olleh")) "goodbye")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1003: "
{-# LINE 1003 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1003 "src/Test/HMock/Predicates.hs" #-}
      (show ($(qWith [| abs |]) (gt 5)))
  [ExpectedLine [LineChunk "\"abs: > 5\""]]
 DocTest.printPrefix "Test.HMock.Predicates:1022: "
{-# LINE 1022 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1022 "src/Test/HMock/Predicates.hs" #-}
      (accept $(qMatch [p| Just (Left _) |]) Nothing)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1024: "
{-# LINE 1024 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1024 "src/Test/HMock/Predicates.hs" #-}
      (accept $(qMatch [p| Just (Left _) |]) (Just (Left 5)))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:1026: "
{-# LINE 1026 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1026 "src/Test/HMock/Predicates.hs" #-}
      (accept $(qMatch [p| Just (Left _) |]) (Just (Right 5)))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1029: "
{-# LINE 1029 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1029 "src/Test/HMock/Predicates.hs" #-}
      (show $(qMatch [p| Just (Left _) |]))
  [ExpectedLine [LineChunk "\"Just (Left _)\""]]
 DocTest.printPrefix "Test.HMock.Predicates:1047: "
{-# LINE 1047 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1047 "src/Test/HMock/Predicates.hs" #-}
      (accept (typed @String anything) "foo")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Predicates:1049: "
{-# LINE 1049 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1049 "src/Test/HMock/Predicates.hs" #-}
      (accept (typed @String (sizeIs (gt 5))) "foo")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Predicates:1051: "
{-# LINE 1051 "src/Test/HMock/Predicates.hs" #-}
 DocTest.example
{-# LINE 1051 "src/Test/HMock/Predicates.hs" #-}
      (accept (typed @String anything) (42 :: Int))
  [ExpectedLine [LineChunk "False"]]
