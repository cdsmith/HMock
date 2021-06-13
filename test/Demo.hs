{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Demo where

import Data.Persist (Persist)

-- This is an in-depth example of using HMock to test a system with dependencies
-- on external systems.  The application here is a chat client, which needs to
-- be able to talk to services for authentication, communication, and storage.

-- We start with some basic types.

-- | Represents a user in the system.
newtype User = User String deriving (Eq, Show)

-- | Represents a permission level.  Guest does not require login, NormalUser
-- requires being logged in, and Admin requires being logged in with elevated
-- permissions.
data PermLevel = Guest | NormalUser | Admin deriving (Eq, Show)

-- | Represents a chat session.
newtype Session = Session Int deriving (Eq, Show)

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
class MonadAuth m where
  login :: String -> String -> m (Either String User)
  logout :: m ()
  withLogin :: String -> String -> (User -> m ()) -> m (Either String ())
  getUser :: m (Maybe User)
  hasPermission :: PermLevel -> m Bool

-- | An MTL-style type class for a shared key-value data store.  For the most
-- part, this could be faked instead of mocked.  However, we may occasionally
-- want to test failure cases that are awkward to fake.
class Persist a => SharedDB a m | m -> a where
  storeDB :: String -> a -> m ()
  lookupDB :: String -> m (Maybe a)

-- | An MTL-style type class for sending and receiving chat messages.  We can
-- start and close sessions, send messages, poll for messages (blocking for a
-- period of time if desired), and ban other users (probably only if we're an
-- admin!)
class MonadAuth m => MonadChat m where
  startSession :: m Session
  closeSession :: Session -> m ()
  sendChat :: Session -> String -> m ()
  pollChat :: Num a => Session -> a -> m [(User, String)]
  ban :: Session -> User -> m ()
