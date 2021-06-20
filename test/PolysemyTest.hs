{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module PolysemyTest where

import Data.Typeable (Typeable)
import Polysemy

data FileSystem (m :: * -> *) a where
  ReadFile :: FilePath -> FileSystem m String
  WriteFile :: FilePath -> String -> FileSystem m ()

makeSem ''FileSystem

class Mockable (eff :: Effect) where
  data Action eff :: (* -> *) -> * -> *

instance Mockable FileSystem where
  data Action FileSystem m a where
    ReadFile_ :: FilePath -> Action FileSystem m String
    WriteFile_ :: FilePath -> String -> Action FileSystem m ()

data Step (m :: * -> *) where
  Step ::
    (Mockable eff, Typeable eff, Typeable m, Typeable a) =>
    Action eff m a ->
    m a ->
    Step m

type ExpectSet m = [Step m]

data Mock m a where
  Expect :: ExpectSet m -> Mock m ()
  MockAction :: Mockable eff => Action eff m a -> Mock m a

makeSem ''Mock

interpretFSToMock :: Member Mock r => Sem (FileSystem ': r) a -> Sem r a
interpretFSToMock = interpret $ \case
    ReadFile f -> mockAction (ReadFile_ f)
    WriteFile f s -> mockAction (WriteFile_ f s)

{-
interpretMock :: forall r a. Sem (Mock ': r) a -> Sem r a
interpretMock = do
    (finalES, r) <- runState [] $ reinterpretH $ \case
        Expect es -> do
            es' <- get
            put (es ++ es')
            return ()
        MockAction a -> error "Unimplemented"
    unless (null finalES) $ error "Unmet expectations"
    return r
-}
