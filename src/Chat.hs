{-|
Module      : Chat
Description : This module includes all the chat related functions
-}
module Chat (
    -- * Functions
    createChat,
    addToChat,
    findChat,
    getChatID,
    resolveChatID,
    sendMessage,
    displayChat,
    getRandomMessage,
) where

import User (getSuids, earlierUser, earlierSuid)
import Types (SocialNetwork(..), User(..), ChatID, Message(..), Chat(..))

import Control.Concurrent (takeMVar, putMVar)
import qualified Data.Map as Map
import System.Random (randomRIO)

-- |The 'createChat' function adds a chat to the map storing all messages
createChat :: SocialNetwork -> ChatID -> IO ()
createChat (SocialNetwork _ _ m _) cid = do
    let newChat = Chat []
    messages <- takeMVar m
    putMVar m (Map.insert cid newChat messages)

-- | The 'addToChat' function allows for a message to be added to a chat instance
addToChat :: Chat -> Message -> Chat
addToChat (Chat c) m = Chat (c ++ [m])

-- |The 'findChat' function looks for a chat between the 2 given user id's
findChat :: SocialNetwork -> String -> String -> IO (Maybe Chat)
findChat (SocialNetwork _ _ m _) suid1 suid2 = do
    let cid = resolveChatID suid1 suid2 
    messages <- takeMVar m
    putMVar m messages
    return (Map.lookup cid messages)

-- |The 'getChatID' function takes two users and returns their corresponding chat's id
getChatID :: User -> User -> ChatID
getChatID u1 u2 = do
    let (suid1, suid2) = getSuids (u1,u2)
    if earlierUser u1 u2 then suid1++suid2 else suid2++suid1

-- |The 'resolveChatID' function takes two user ids as strings and returns their corresponding chat's id
resolveChatID :: String -> String -> ChatID
resolveChatID suid1 suid2 = if earlierSuid suid1 suid2 then suid1++suid2 else suid2++suid1

-- |The 'sendMessage' function adds a message to a chat between two users
sendMessage :: SocialNetwork -> User -> User -> String -> IO ()
sendMessage (SocialNetwork _ _ m _) u1 u2 text = do
    let b = earlierUser u1 u2
    let cid = getChatID u1 u2
    let message = Message u1 u2 b text
    messages <- takeMVar m
    let currChat = Map.lookup cid messages
    case currChat of
        Just chat -> do
            let newChat = addToChat chat message
            putMVar m (Map.insert cid newChat messages)
        Nothing -> do
            putMVar m (Map.insert cid (Chat [message]) messages)

-- |The 'displayChat' function takes a (Maybe Chat) and pretty prints it
displayChat :: Maybe Chat -> IO ()
displayChat x = case x of
    Just chat -> do print chat
    Nothing -> print "Empty chat"

-- |The 'getRandomMessage' function randomly chooses from the list of available messages
getRandomMessage :: IO String
getRandomMessage = do
    allMessages <- readFile "data/messages.txt"
    messageIndex <- randomRIO (0,600) :: IO Int
    return $ lines allMessages !! messageIndex