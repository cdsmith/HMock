{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

import HMock
import HMock.TH
import Test.Hspec hiding (Expectation)
import Prelude hiding (readFile, writeFile)
import qualified Prelude

class Monad m => MonadFS m where
  readFile :: FilePath -> m String
  writeFile :: FilePath -> String -> m ()

instance MonadFS IO where
  readFile = Prelude.readFile
  writeFile = Prelude.writeFile

makeMockable ''MonadFS

copyFile :: MonadFS m => FilePath -> FilePath -> m ()
copyFile a b = readFile a >>= writeFile b

main :: IO ()
main = hspec $ do
  describe "copyFile" $ do
    it "copies the file" $
      example . runMockT $ do
        mock $ expect $ ReadFile "foo.txt" |-> "lorem ipsum"
        mock $ expect $ WriteFile "bar.txt" "lorem ipsum" |-> ()

        copyFile "foo.txt" "bar.txt"
