-- Do not edit! Automatically created with doctest-extract from src/Test/HMock/Internal/Predicates.hs
{-# LINE 24 "src/Test/HMock/Internal/Predicates.hs" #-}

{-# OPTIONS_GHC -XTemplateHaskell #-}
{-# OPTIONS_GHC -XTypeApplications #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module DocTests.Test.HMock.Internal.Predicates where

import Test.HMock.Internal.Predicates
import Test.DocTest.Base
import qualified Test.DocTest.Driver as DocTest

{-# LINE 28 "src/Test/HMock/Internal/Predicates.hs" #-}

test :: DocTest.T ()
test = do
 DocTest.printPrefix "Test.HMock.Internal.Predicates:44: "
{-# LINE 44 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 44 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept anything "foo")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:46: "
{-# LINE 46 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 46 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept anything undefined)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:57: "
{-# LINE 57 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 57 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (eq "foo") "foo")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:59: "
{-# LINE 59 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 59 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (eq "foo") "bar")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:70: "
{-# LINE 70 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 70 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (neq "foo") "foo")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:72: "
{-# LINE 72 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 72 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (neq "foo") "bar")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:83: "
{-# LINE 83 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 83 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (gt 5) 4)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:85: "
{-# LINE 85 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 85 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (gt 5) 5)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:87: "
{-# LINE 87 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 87 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (gt 5) 6)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:99: "
{-# LINE 99 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 99 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (geq 5) 4)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:101: "
{-# LINE 101 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 101 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (geq 5) 5)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:103: "
{-# LINE 103 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 103 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (geq 5) 6)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:114: "
{-# LINE 114 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 114 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (lt 5) 4)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:116: "
{-# LINE 116 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 116 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (lt 5) 5)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:118: "
{-# LINE 118 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 118 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (lt 5) 6)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:129: "
{-# LINE 129 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 129 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (leq 5) 4)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:131: "
{-# LINE 131 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 131 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (leq 5) 5)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:133: "
{-# LINE 133 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 133 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (leq 5) 6)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:144: "
{-# LINE 144 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 144 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept true True)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:146: "
{-# LINE 146 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 146 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept true False)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:153: "
{-# LINE 153 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 153 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept false True)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:155: "
{-# LINE 155 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 155 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept false False)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:163: "
{-# LINE 163 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 163 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (just (eq "value")) Nothing)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:165: "
{-# LINE 165 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 165 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (just (eq "value")) (Just "value"))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:167: "
{-# LINE 167 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 167 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (just (eq "value")) (Just "wrong value"))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:179: "
{-# LINE 179 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 179 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (left (eq "value")) (Left "value"))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:181: "
{-# LINE 181 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 181 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (left (eq "value")) (Right "value"))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:183: "
{-# LINE 183 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 183 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (left (eq "value")) (Left "wrong value"))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:195: "
{-# LINE 195 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 195 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (right (eq "value")) (Right "value"))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:197: "
{-# LINE 197 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 197 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (right (eq "value")) (Right "wrong value"))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:199: "
{-# LINE 199 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 199 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (right (eq "value")) (Left "value"))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:211: "
{-# LINE 211 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 211 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (zipP (eq "foo") (eq "bar")) ("foo", "bar"))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:213: "
{-# LINE 213 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 213 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (zipP (eq "foo") (eq "bar")) ("bar", "foo"))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:225: "
{-# LINE 225 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 225 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (zip3P (eq "foo") (eq "bar") (eq "qux")) ("foo", "bar", "qux"))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:227: "
{-# LINE 227 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 227 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (zip3P (eq "foo") (eq "bar") (eq "qux")) ("qux", "bar", "foo"))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:239: "
{-# LINE 239 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 239 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (zip4P (eq 1) (eq 2) (eq 3) (eq 4)) (1, 2, 3, 4))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:241: "
{-# LINE 241 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 241 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (zip4P (eq 1) (eq 2) (eq 3) (eq 4)) (4, 3, 2, 1))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:259: "
{-# LINE 259 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 259 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (zip5P (eq 1) (eq 2) (eq 3) (eq 4) (eq 5)) (1, 2, 3, 4, 5))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:261: "
{-# LINE 261 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 261 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (zip5P (eq 1) (eq 2) (eq 3) (eq 4) (eq 5)) (5, 4, 3, 2, 1))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:280: "
{-# LINE 280 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 280 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (lt "foo" `andP` gt "bar") "eta")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:282: "
{-# LINE 282 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 282 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (lt "foo" `andP` gt "bar") "quz")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:284: "
{-# LINE 284 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 284 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (lt "foo" `andP` gt "bar") "alpha")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:295: "
{-# LINE 295 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 295 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (lt "bar" `orP` gt "foo") "eta")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:297: "
{-# LINE 297 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 297 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (lt "bar" `orP` gt "foo") "quz")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:299: "
{-# LINE 299 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 299 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (lt "bar" `orP` gt "foo") "alpha")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:311: "
{-# LINE 311 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 311 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (notP (eq "negative")) "positive")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:313: "
{-# LINE 313 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 313 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (notP (eq "negative")) "negative")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:324: "
{-# LINE 324 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 324 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (startsWith "fun") "fungible")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:326: "
{-# LINE 326 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 326 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (startsWith "gib") "fungible")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:337: "
{-# LINE 337 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 337 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (endsWith "ow") "crossbow")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:339: "
{-# LINE 339 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 339 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (endsWith "ow") "trebuchet")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:351: "
{-# LINE 351 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 351 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (hasSubstr "i") "team")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:353: "
{-# LINE 353 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 353 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (hasSubstr "i") "partnership")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:365: "
{-# LINE 365 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 365 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (hasSubsequence [1..5]) [1, 2, 3, 4, 5])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:367: "
{-# LINE 367 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 367 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (hasSubsequence [1..5]) [0, 1, 0, 2, 0, 3, 0, 4, 0, 5, 0])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:369: "
{-# LINE 369 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 369 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (hasSubsequence [1..5]) [2, 3, 5, 7, 11])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:381: "
{-# LINE 381 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 381 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (caseInsensitive startsWith "foo") "FOOTBALL!")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:383: "
{-# LINE 383 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 383 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (caseInsensitive endsWith "ball") "soccer")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:385: "
{-# LINE 385 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 385 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (caseInsensitive eq "time") "TIME")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:387: "
{-# LINE 387 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 387 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (caseInsensitive gt "NOTHING") "everything")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:412: "
{-# LINE 412 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 412 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (matchesRegex "x{2,5}y?") "xxxy")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:414: "
{-# LINE 414 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 414 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (matchesRegex "x{2,5}y?") "xyy")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:416: "
{-# LINE 416 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 416 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (matchesRegex "x{2,5}y?") "wxxxyz")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:439: "
{-# LINE 439 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 439 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (matchesCaseInsensitiveRegex "x{2,5}y?") "XXXY")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:441: "
{-# LINE 441 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 441 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (matchesCaseInsensitiveRegex "x{2,5}y?") "XYY")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:443: "
{-# LINE 443 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 443 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (matchesCaseInsensitiveRegex "x{2,5}y?") "WXXXYZ")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:472: "
{-# LINE 472 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 472 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (containsRegex "x{2,5}y?") "xxxy")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:474: "
{-# LINE 474 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 474 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (containsRegex "x{2,5}y?") "xyy")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:476: "
{-# LINE 476 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 476 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (containsRegex "x{2,5}y?") "wxxxyz")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:497: "
{-# LINE 497 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 497 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (containsCaseInsensitiveRegex "x{2,5}y?") "XXXY")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:499: "
{-# LINE 499 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 499 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (containsCaseInsensitiveRegex "x{2,5}y?") "XYY")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:501: "
{-# LINE 501 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 501 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (containsCaseInsensitiveRegex "x{2,5}y?") "WXXXYZ")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:522: "
{-# LINE 522 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 522 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept isEmpty [])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:524: "
{-# LINE 524 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 524 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept isEmpty [1, 2, 3])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:526: "
{-# LINE 526 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 526 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept isEmpty "")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:528: "
{-# LINE 528 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 528 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept isEmpty "gas tank")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:539: "
{-# LINE 539 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 539 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept nonEmpty [])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:541: "
{-# LINE 541 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 541 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept nonEmpty [1, 2, 3])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:543: "
{-# LINE 543 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 543 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept nonEmpty "")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:545: "
{-# LINE 545 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 545 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept nonEmpty "gas tank")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:557: "
{-# LINE 557 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 557 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (sizeIs (lt 3)) ['a' .. 'f'])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:559: "
{-# LINE 559 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 559 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (sizeIs (lt 3)) ['a' .. 'b'])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:571: "
{-# LINE 571 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 571 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (elemsAre [lt 3, lt 4, lt 5]) [2, 3, 4])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:573: "
{-# LINE 573 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 573 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (elemsAre [lt 3, lt 4, lt 5]) [2, 3, 4, 5])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:575: "
{-# LINE 575 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 575 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (elemsAre [lt 3, lt 4, lt 5]) [2, 10, 4])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:589: "
{-# LINE 589 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 589 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (unorderedElemsAre [eq 1, eq 2, eq 3]) [1, 2, 3])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:591: "
{-# LINE 591 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 591 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (unorderedElemsAre [eq 1, eq 2, eq 3]) [2, 3, 1])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:593: "
{-# LINE 593 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 593 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (unorderedElemsAre [eq 1, eq 2, eq 3]) [1, 2, 3, 4])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:595: "
{-# LINE 595 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 595 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (unorderedElemsAre [eq 1, eq 2, eq 3]) [1, 3])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:611: "
{-# LINE 611 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 611 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (each (gt 5)) [4, 5, 6])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:613: "
{-# LINE 613 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 613 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (each (gt 5)) [6, 7, 8])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:615: "
{-# LINE 615 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 615 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (each (gt 5)) [])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:627: "
{-# LINE 627 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 627 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (contains (gt 5)) [3, 4, 5])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:629: "
{-# LINE 629 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 629 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (contains (gt 5)) [4, 5, 6])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:631: "
{-# LINE 631 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 631 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (contains (gt 5)) [])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:645: "
{-# LINE 645 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 645 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (containsAll [eq "foo", eq "bar"]) ["bar", "foo"])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:647: "
{-# LINE 647 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 647 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (containsAll [eq "foo", eq "bar"]) ["foo"])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:649: "
{-# LINE 649 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 649 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (containsAll [eq "foo", eq "bar"]) ["foo", "bar", "qux"])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:662: "
{-# LINE 662 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 662 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (containsOnly [eq "foo", eq "bar"]) ["foo", "foo"])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:664: "
{-# LINE 664 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 664 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (containsOnly [eq "foo", eq "bar"]) ["foo", "bar"])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:666: "
{-# LINE 666 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 666 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (containsOnly [eq "foo", eq "bar"]) ["foo", "qux"])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:678: "
{-# LINE 678 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 678 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (containsKey (eq "foo")) [("foo", 1), ("bar", 2)])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:680: "
{-# LINE 680 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 680 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (containsKey (eq "foo")) [("bar", 1), ("qux", 2)])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:693: "
{-# LINE 693 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 693 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (containsEntry (eq "foo") (gt 10)) [("foo", 12), ("bar", 5)])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:695: "
{-# LINE 695 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 695 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (containsEntry (eq "foo") (gt 10)) [("foo", 5), ("bar", 12)])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:697: "
{-# LINE 697 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 697 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (containsEntry (eq "foo") (gt 10)) [("bar", 12)])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:710: "
{-# LINE 710 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 710 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (keysAre [eq "a", eq "b", eq "c"]) [("a", 1), ("b", 2), ("c", 3)])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:712: "
{-# LINE 712 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 712 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (keysAre [eq "a", eq "b", eq "c"]) [("c", 1), ("b", 2), ("a", 3)])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:714: "
{-# LINE 714 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 714 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (keysAre [eq "a", eq "b", eq "c"]) [("a", 1), ("c", 3)])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:716: "
{-# LINE 716 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 716 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (keysAre [eq "a", eq "b"]) [("a", 1), ("b", 2), ("c", 3)])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:732: "
{-# LINE 732 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 732 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (entriesAre [(eq 1, eq 2), (eq 3, eq 4)]) [(1, 2), (3, 4)])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:734: "
{-# LINE 734 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 734 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (entriesAre [(eq 1, eq 2), (eq 3, eq 4)]) [(3, 4), (1, 2)])
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:736: "
{-# LINE 736 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 736 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (entriesAre [(eq 1, eq 2), (eq 3, eq 4)]) [(1, 4), (3, 2)])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:738: "
{-# LINE 738 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 738 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (entriesAre [(eq 1, eq 2), (eq 3, eq 4)]) [(1, 2), (3, 4), (5, 6)])
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:759: "
{-# LINE 759 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 759 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (eq 1.0) (sum (replicate 100 0.01)))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:766: "
{-# LINE 766 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 766 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (approxEq 1.0) (sum (replicate 100 0.01)))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:768: "
{-# LINE 768 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 768 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (approxEq 1.0) (sum (replicate 100 0.009999)))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:781: "
{-# LINE 781 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 781 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept finite 1.0)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:783: "
{-# LINE 783 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 783 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept finite (0 / 0))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:785: "
{-# LINE 785 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 785 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept finite (1 / 0))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:796: "
{-# LINE 796 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 796 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept infinite 1.0)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:798: "
{-# LINE 798 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 798 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept infinite (0 / 0))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:800: "
{-# LINE 800 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 800 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept infinite (1 / 0))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:811: "
{-# LINE 811 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 811 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept nAn 1.0)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:813: "
{-# LINE 813 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 813 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept nAn (0 / 0))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:815: "
{-# LINE 815 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 815 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept nAn (1 / 0))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:828: "
{-# LINE 828 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 828 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (is even) 3)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:830: "
{-# LINE 830 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 830 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (is even) 4)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:843: "
{-# LINE 843 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 843 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept $$(qIs [|| even ||]) 3)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:845: "
{-# LINE 845 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 845 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept $$(qIs [|| even ||]) 4)
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:848: "
{-# LINE 848 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 848 "src/Test/HMock/Internal/Predicates.hs" #-}
      (show $$(qIs [|| even ||]))
  [ExpectedLine [LineChunk "\"even\""]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:862: "
{-# LINE 862 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 862 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (with abs (gt 5)) (-6))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:864: "
{-# LINE 864 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 864 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (with abs (gt 5)) (-5))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:866: "
{-# LINE 866 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 866 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (with reverse (eq "olleh")) "hello")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:868: "
{-# LINE 868 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 868 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (with reverse (eq "olleh")) "goodbye")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:882: "
{-# LINE 882 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 882 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept ($$(qWith [|| abs ||]) (gt 5)) (-6))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:884: "
{-# LINE 884 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 884 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept ($$(qWith [|| abs ||]) (gt 5)) (-5))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:886: "
{-# LINE 886 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 886 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept ($$(qWith [|| reverse ||]) (eq "olleh")) "hello")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:888: "
{-# LINE 888 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 888 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept ($$(qWith [|| reverse ||]) (eq "olleh")) "goodbye")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:891: "
{-# LINE 891 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 891 "src/Test/HMock/Internal/Predicates.hs" #-}
      (show ($$(qWith [|| abs ||]) (gt 5)))
  [ExpectedLine [LineChunk "\"abs: > 5\""]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:908: "
{-# LINE 908 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 908 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept $(qMatch [p| Just (Left _) |]) Nothing)
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:910: "
{-# LINE 910 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 910 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept $(qMatch [p| Just (Left _) |]) (Just (Left 5)))
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:912: "
{-# LINE 912 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 912 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept $(qMatch [p| Just (Left _) |]) (Just (Right 5)))
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:915: "
{-# LINE 915 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 915 "src/Test/HMock/Internal/Predicates.hs" #-}
      (show $(qMatch [p| Just (Left _) |]))
  [ExpectedLine [LineChunk "\"Just (Left _)\""]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:931: "
{-# LINE 931 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 931 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (typed @String anything) "foo")
  [ExpectedLine [LineChunk "True"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:933: "
{-# LINE 933 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 933 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (typed @String (sizeIs (gt 5))) "foo")
  [ExpectedLine [LineChunk "False"]]
 DocTest.printPrefix "Test.HMock.Internal.Predicates:935: "
{-# LINE 935 "src/Test/HMock/Internal/Predicates.hs" #-}
 DocTest.example
{-# LINE 935 "src/Test/HMock/Internal/Predicates.hs" #-}
      (accept (typed @String anything) (42 :: Int))
  [ExpectedLine [LineChunk "False"]]
