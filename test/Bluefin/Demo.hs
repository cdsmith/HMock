{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | An in-depth example of testing a Bluefin program with HMock.  The program
-- is a chat bot for an IRC-style network.  We test its logic against an
-- abstraction of the chat protocol, without a real server.
module Bluefin.Demo (bluefinDemoSpec) where

import Bluefin.Compound
  ( Generic,
    Handle,
    OneWayCoercible (..),
    OneWayCoercibleHandle (..),
    gOneWayCoercible,
    mapHandle,
  )
import Bluefin.Eff (Eff, runEff, type (<:))
import Bluefin.State (evalState, get, modify)
import Control.Monad (forM_)
import Test.HMock.Bluefin
import Test.Hspec (SpecWith, describe, it, shouldBe)
import Test.Predicates (anything)

-------------------------------------------------------------------------------
-- PART 1: THE PROTOCOL ABSTRACTION

-- A real IRC connection is a line-based socket: you write @JOIN #haskell@ and
-- read back lines like @:bob!…PRIVMSG #haskell :hello@.  None of the bot's
-- logic should care about that wire format.  So we capture just what the bot
-- needs as a Bluefin capability: a record of effectful operations, passed
-- around as an ordinary value.  Its implementation would speak IRC; the tests
-- below never build one.

newtype Nick = Nick String deriving (Eq, Show)

newtype Channel = Channel String deriving (Eq, Show)

nickName :: Nick -> String
nickName (Nick n) = n

-- | Something the bot hears from the server.  A real connection would decode
-- these from protocol lines.
data Event
  = -- | Someone spoke in a channel.
    Said Nick Channel String
  | -- | A keepalive the server expects to be answered with a 'pong'.
    Pinged String
  deriving (Eq, Show)

-- | A connection to one chat network.  As is conventional for a
-- "Bluefin.Compound" capability, the record fields are named @…Impl@ and
-- callers go through the wrappers below, so the plain names stay free.
data Chat e = MkChat
  { joinChannelImpl :: Channel -> Eff e (),
    sayImpl :: Channel -> String -> Eff e (),
    pongImpl :: String -> Eff e (),
    -- | Blocks until the next event arrives.
    nextEventImpl :: Eff e Event
  }
  deriving (Generic)
  deriving (Handle) via OneWayCoercibleHandle Chat

instance (e <: es) => OneWayCoercible (Chat e) (Chat es) where
  oneWayCoercibleImpl = gOneWayCoercible

joinChannel :: (e <: es) => Chat e -> Channel -> Eff es ()
joinChannel chat = joinChannelImpl (mapHandle chat)

say :: (e <: es) => Chat e -> Channel -> String -> Eff es ()
say chat = sayImpl (mapHandle chat)

pong :: (e <: es) => Chat e -> String -> Eff es ()
pong chat = pongImpl (mapHandle chat)

nextEvent :: (e <: es) => Chat e -> Eff es Event
nextEvent chat = nextEventImpl (mapHandle chat)

-------------------------------------------------------------------------------
-- PART 2: THE BOT

-- The code under test.  It works entirely in terms of the 'Chat' abstraction,
-- so it runs the same against a real server or against a mock.

-- | Joins the given channels, then reacts to events until an operator says
-- @!quit@.  Answers keepalives so the connection stays open.
serve :: (e <: es) => Chat e -> [Channel] -> Eff es ()
serve chat channels = do
  forM_ channels (joinChannel chat)
  loop
  where
    loop = do
      event <- nextEvent chat
      case event of
        Pinged token -> pong chat token >> loop
        Said who where_ text -> react who where_ text
    react who where_ text = case words text of
      ["!quit"] -> return ()
      ["!hello"] -> say chat where_ ("Hello, " ++ nickName who ++ "!") >> loop
      ("!echo" : rest) -> say chat where_ (unwords rest) >> loop
      _ -> loop

-- | Mirrors ordinary chatter in a channel from one network to another.  Bot
-- commands (anything starting with @!@) are left where they are said.
relay :: (e1 <: es, e2 <: es) => Chat e1 -> Chat e2 -> Channel -> Eff es ()
relay from to channel@(Channel target) = loop
  where
    loop = do
      event <- nextEvent from
      case event of
        Said _ _ "!quit" -> return ()
        Said who (Channel where_) text
          | where_ == target,
            not (isCommand text) ->
              say to channel (nickName who ++ ": " ++ text) >> loop
        _ -> loop
    isCommand ('!' : _) = True
    isCommand _ = False

-------------------------------------------------------------------------------
-- PART 3: MOCKS

-- One splice generates everything HMock needs to mock a 'Chat': an Action and
-- a Matcher constructor for each operation and 'mocked', which builds a 'Chat'
-- record that delegates to the mock.  The @…Impl@ suffix is stripped, so the
-- mock is named for the operation: @JoinChannel@, @Say@, @Pong@, @NextEvent@,
-- and their @_@ matchers.
makeMockableCapability [t|Chat|]

-- | Creates a connection mock with the behaviour every test wants unless it
-- says otherwise.  Joining channels and answering keepalives are plumbing that
-- most tests don't assert, so allow them silently; a test that cares can still
-- add its own expectation on top.
newMockChat ::
  (e <: es) =>
  MockSource root resp e ->
  Eff es (Mock root resp Chat e)
newMockChat source = do
  chat <- newMock source
  allowUnexpected chat $ JoinChannel_ anything
  allowUnexpected chat $ Pong_ anything
  return chat

-------------------------------------------------------------------------------
-- PART 4: TESTS

bluefinDemoSpec :: SpecWith ()
bluefinDemoSpec = describe "chat bot" $ do
  it "greets a user who says !hello" $
    runEff $ \io -> runMock io $ \source -> do
      chat <- newMockChat source

      -- nextEvent has no arguments, so its Action constructor is nullary and a
      -- chain of responses scripts the stream the bot reads.
      expectAny chat $
        NextEvent
          |-> Said (Nick "bob") (Channel "#haskell") "!hello"
          |-> Said (Nick "bob") (Channel "#haskell") "!quit"
      expect chat $ Say (Channel "#haskell") "Hello, bob!"

      serve (mocked chat) [Channel "#haskell"]

  it "echoes back what it is told" $
    runEff $ \io -> runMock io $ \source -> do
      chat <- newMockChat source
      expectAny chat $
        NextEvent
          |-> Said (Nick "bob") (Channel "#haskell") "!echo one two three"
          |-> Said (Nick "bob") (Channel "#haskell") "!quit"
      expect chat $ Say (Channel "#haskell") "one two three"

      serve (mocked chat) [Channel "#haskell"]

  it "answers a server ping so the connection stays alive" $
    runEff $ \io -> runMock io $ \source -> do
      chat <- newMockChat source
      expectAny chat $
        NextEvent
          |-> Pinged "LAG42"
          |-> Said (Nick "op") (Channel "#ops") "!quit"
      -- pong is allowed by default, but here we insist on the exact token.
      expect chat $ Pong "LAG42"

      serve (mocked chat) []

  it "joins each of its channels on startup, in order" $
    runEff $ \io -> runMock io $ \source -> do
      chat <- newMockChat source
      inSequence
        [ expect chat $ JoinChannel (Channel "#haskell"),
          expect chat $ JoinChannel (Channel "#bluefin")
        ]
      expectAny chat $ NextEvent |-> Said (Nick "op") (Channel "#ops") "!quit"

      serve (mocked chat) [Channel "#haskell", Channel "#bluefin"]

  it "keeps a transcript of everything it says" $ do
    -- A response runs in Eff, so it can use a handle from outside runMock --
    -- here a piece of State that collects what the bot said.
    transcript <- runEff $ \io -> evalState [] $ \journal -> do
      runMock io $ \source -> do
        chat <- newMockChat source
        allowUnexpected chat $
          Say_ anything anything |=> \(Say _ message) -> modify journal (++ [message])
        expectAny chat $
          NextEvent
            |-> Said (Nick "bob") (Channel "#haskell") "!hello"
            |-> Said (Nick "bob") (Channel "#haskell") "!echo hi there"
            |-> Said (Nick "bob") (Channel "#haskell") "!quit"
        serve (mocked chat) [Channel "#haskell"]
      get journal
    transcript `shouldBe` ["Hello, bob!", "hi there"]

  describe "relay between two networks" $ do
    -- Two Chats are two instances of one capability.  Each has its own
    -- expectations, and a call through one never satisfies the other's.
    it "mirrors chatter from one network to the other" $
      runEff $ \io -> runMock io $ \source -> do
        libera <- newMockChat source
        oftc <- newMockChat source

        expectAny libera $
          NextEvent
            |-> Said (Nick "bob") (Channel "#haskell") "hello, world"
            |-> Said (Nick "bob") (Channel "#haskell") "!quit"
        expect oftc $ Say (Channel "#haskell") "bob: hello, world"

        relay (mocked libera) (mocked oftc) (Channel "#haskell")

    it "does not mirror bot commands" $
      runEff $ \io -> runMock io $ \source -> do
        libera <- newMockChat source
        oftc <- newMockChat source

        -- oftc expects no 'say' at all: a mirrored command would fail here as
        -- an unexpected action on instance oftc.
        expectAny libera $
          NextEvent
            |-> Said (Nick "bob") (Channel "#haskell") "!hello"
            |-> Said (Nick "bob") (Channel "#haskell") "!quit"

        relay (mocked libera) (mocked oftc) (Channel "#haskell")
