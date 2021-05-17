import Test.DocTest (doctest)

main :: IO ()
main =
  doctest
    [ "-isrc",
      "src/Test/HMock/Internal/Predicates.hs",
      "src/Test/HMock/Internal/Multiplicity.hs"
    ]
