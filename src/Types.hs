-- {-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Types
Description : This module includes all the type definitions required throughout the application
-}
module Types (
    -- * Types
    UserID,
    User (..),
    Userbase,
    Message(..),
    Chat (..),
    Communication (..),
    ChatID,
    Messages,
    SocialNetwork (..),
    Key,
    Value,
) where

import Data.Map (Map)
import Control.Concurrent (MVar)
import Data.Text(justifyRight, pack, unpack)

-- | The UserID type represents strings to be used as keys for Userbase entries
type UserID = String

-- | This is the data structure that will define a User type
data User = User {
    -- | The 'userid' method returns a User's id
    userid :: UserID,
    -- | The 'username' method returns a User's username
    username :: String
}

-- | Defining User's behaviour for how to display a user instance
instance Show User where
    show u = username u ++ "#" ++ userid u

-- | The Message type will hold the means with which two users interact
data Message = Message User User Bool String

-- | 'justify' equally justifies any string that should be justified
justify :: String -> String
justify text = unpack $ justifyRight 100 ' ' $ pack text

-- | Message should be displayed as text justified to the left or right according to the user's priority in a chat
instance Show Message where
    show (Message u _ b m) = let (userline, messageLine) = if b then (show u ++ ":", m) else (justify $ ":" ++ show u, justify m) in
        "\n" ++ userline ++ "\n" ++ messageLine ++ "\n"

-- | The Chat type will keep track of a full interaction between two users
newtype Chat = Chat [Message]

-- | Defining Chat as an instance of show to set the desired output
instance Show Chat where
    show (Chat []) = ""
    show (Chat c) = show (head c) ++ show (Chat $ tail c)
    -- show (Chat c) = unlines c -- REVISIT

-- | A Communication data structure will help map user id's to the message record between them
data Communication = Communication User User

-- | A ChatID type will be generated by a communication structure and be used as key to find an interaction between two users
type ChatID = String

-- | The Messages type is a map containing all messages between users in this socialnetwork 
type Messages = Map ChatID Chat

-- | The Key type is a type for strings to be used as map entries' lookup id's
type Key = String

-- | The Value type is a type for strings to be used as map entries' values
type Value = String

-- | The Database type is a type for any sort of more generic data required to be kept for the socialnetwork to function
type Database = Map Key Value

-- | The Userbase type is a map to store all users registered in the socialnetwork
type Userbase = Map UserID User

-- | The SocialNetwork will be accessible by each user thread so that users can send messages through it and alter its state
data SocialNetwork = SocialNetwork (MVar Database) (MVar Userbase) (MVar Messages) (MVar ()) -- REVISIT maybe adding an 'Atomic Operations' mvar to make sure messages don't go over 100