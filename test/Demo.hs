{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Demo where

import Control.Monad.State
import Data.Char (isLetter)
import Data.Functor ((<&>))
import Data.IORef (modifyIORef, newIORef, readIORef)
import qualified Data.Map as Map
import Data.Typeable (Typeable)
import Test.HMock
import Test.HMock.TH
import Test.Hspec
import Prelude hiding (appendFile, readFile, writeFile)

-- This is an in-depth example of using HMock to test a system with dependencies
-- on external systems.  The application here is a chat bot, which needs to
-- be able to talk to services for authentication, communication, and storage.

-------------------------------------------------------------------------------
-- PART 1: TYPES AND CLASSES

-- We start with some basic types.

-- | Represents a user in the system.
newtype User = User String deriving (Eq, Show)

-- | Represents a permission level.  The Guest level is given to users who are
-- not logged in.  NormalUser requires being logged in.  Admin requires being
-- logged in with elevated permissions.
data PermLevel = Guest | NormalUser | Admin deriving (Eq, Show)

-- | Represents a chat room.
newtype Room = Room String deriving (Eq, Show)

-- HMock needs MTL-style type classes to mock.  Here, we implement a number of
-- these classes.

-- | An MTL-style type class to capture authentication.  This can be used in two
-- ways.
--
-- 1. Using the 'login' and 'logout' actions to start and end an authenticated
--    session.  While the user is logged in, 'getUser' and 'hasPermission' will
--    return appropriate responses for that user.  Remember to log out!
--
-- 2. Using the 'withLogin' action to run an action as a user.  The user will
--    be logged out automatically when the action finishes.
class Monad m => MonadAuth m where
  login :: String -> String -> m User
  logout :: m ()
  hasPermission :: PermLevel -> m Bool

-- | An MTL-style type class for sending and receiving chat messages.  We can
-- start and close sessions, send messages, poll for messages (blocking for a
-- period of time if desired), and ban other users (probably only if we're an
-- admin!)
class MonadAuth m => MonadChat m where
  joinRoom :: String -> m Room
  leaveRoom :: Room -> m ()
  sendChat :: Room -> String -> m ()
  pollChat :: Room -> m (User, String)
  ban :: Room -> User -> m ()

-- | An MTL-style type class for accessing the filesystem.  It wouldn't be a
-- good idea to write mock-style tests for all uses of the filesystem, but we
-- can use HMock to set up a lightweight fake.
class Monad m => MonadFilesystem m where
  readFile :: FilePath -> m String
  writeFile :: FilePath -> String -> m ()
  appendFile :: FilePath -> String -> m ()

-------------------------------------------------------------------------------
-- PART 2: IMPLEMENTATION

-- We will now implement a simple chatbot, which joins a chat room and responds
-- to messages.

chatbot :: (MonadAuth m, MonadChat m, MonadFilesystem m) => m ()
chatbot = do
  _ <- login "HMockBot" "secretish"
  room <- joinRoom "#haskell"
  listenAndReply room
  leaveRoom room
  logout

listenAndReply :: (MonadAuth m, MonadChat m, MonadFilesystem m) => Room -> m ()
listenAndReply room = do
  (user, msg) <- pollChat room
  finished <- case words msg of
    ["!leave"] -> return True
    ("!reportbug" : ws) -> do
      appendFile "bugs.txt" (unwords ws ++ "\n")
      sendChat room "Thanks for the bug report!"
      return False
    ws | any ((== 4) . length . filter isLetter) ws -> do
      isAdmin <- hasPermission Admin
      when isAdmin $ do
        ban room user
        sendChat room "Sorry for the disturbance!"
      return False
    _ -> return False
  unless finished (listenAndReply room)

-------------------------------------------------------------------------------
-- PART 3: MOCKS

-- Set up the mocks for the three classes.  makeMockable is the first step to
-- using HMock, and defines a number of boilerplate types and instances that are
-- used by the framework and your tests.

makeMockable ''MonadAuth
makeMockable ''MonadChat
makeMockable ''MonadFilesystem

-- This sets up a lightweight fake for the database, using the state monad.
-- This isn't really mocking in the traditional sense of the word, but using
-- HMock for lightweight fakes saves the need to define a new type and instance,
-- and allows the flexibility to easily inject failures and make assertions
-- about behavior when it's important to do so.
--
-- We could do this with a State monad, but it's actually more modular here to
-- use an IORef, so that our client doesn't need to handle setting up the state.
fakeFS :: (Typeable m, MonadIO m) => MockT m ()
fakeFS = do
  fs <- liftIO $ newIORef Map.empty
  whenever $
    ReadFile_ anything
      |=> \(ReadFile file) -> liftIO (readIORef fs) <&> (Map.! file)
  whenever $
    WriteFile_ anything anything
      |=> \(WriteFile file string) ->
        liftIO $ modifyIORef fs $ Map.insert file string
  whenever $
    AppendFile_ anything anything
      |=> \(AppendFile file string) ->
        liftIO $ modifyIORef fs $ Map.update (Just . (++ string)) file

-- Here, we set up some general expectations
setup :: (Typeable m, MonadIO m) => MockT m ()
setup = do
  -- Use a fake filesystem
  fakeFS
  whenever $ HasPermission_ anything |-> True

-------------------------------------------------------------------------------
-- PART 4: TESTS

-- We're now ready to write tests for the behavior of our chat bot.  We'll just
-- write a few representative tests to see how things work.  I'm using Hspec for
-- the test framework, but you can use your favorite framework.

demoSpec :: SpecWith ()
demoSpec = describe "chatbot" $ do
  it "bans users who use four-letter words" $
    example $
      runMockT $ do
        setup
        inSequence
          [ expect $ Login "HMockBot" "secretish",
            expect $ JoinRoom "#haskell" |-> Room "#haskell",
            expect $ LeaveRoom (Room "#haskell"),
            expect Logout
          ]
        expect $
          PollChat_ anything
            |-> (User "A", "I love Haskell")
            |-> (User "B", "Lovin' the ass. candies")
            |-> (User "B", "!leave")
        expect $ Ban (Room "#haskell") (User "A")
        whenever $ SendChat_ anything anything
        chatbot