{-|
Module      : Chat
Description : This module includes all the chat related functions
-}
module Chat (
    -- * Functions
    createChat,
    findChat,
    sendMessage,
    displayChat,
    getRandomMessage,
) where

import User (getSuids, earlierUser)
import Types (
    -- * Types  -- REVISIT
    SocialNetwork(..), User(..), ChatID, Message(..), Chat(..),
    -- * Functions
    addToChat,
    )


import Control.Concurrent (takeMVar, putMVar)
import qualified Data.Map as Map
import System.Random (randomRIO)


-- |The 'createChat' function adds a chat to the map storing all messages
createChat :: SocialNetwork -> ChatID -> IO ()
createChat (SocialNetwork _ _ m _) cid = do
    let newChat = Chat []
    messages <- takeMVar m
    putMVar m (Map.insert cid newChat messages)
    -- Potential improvement to avoid space leaks
    -- let messages' = Map.insert cid newChat messages
    -- putMVar m messages'
    -- seq messages' (return ())

-- |The 'findChat' function looks up for a chat with the given chatid as key
findChat :: SocialNetwork -> ChatID -> IO (Maybe Chat)
findChat (SocialNetwork _ _ m _) cid = do
    messages <- takeMVar m
    putMVar m messages
    return (Map.lookup cid messages)

-- |The 'getChatID' function takes two users and returns their corresponding chat's id
getChatID :: User -> User -> ChatID
getChatID u1 u2 = do
    let (suid1, suid2) = getSuids (u1,u2)
    if earlierUser u1 u2 then suid1++suid2 else suid2++suid1

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
            -- putStrLn $ "\nInserting message:\n" ++ text ++ "\ninto chat: " ++ cid ++ "\n\n"
            putMVar m (Map.insert cid newChat messages)
        Nothing -> do
            putMVar m (Map.insert cid (Chat [message]) messages)

-- -- |The 'sendMessage' function adds a message to a chat between two users -- REVISIT
-- sendMessage :: SocialNetwork -> ChatID -> Message -> IO ()
-- sendMessage (SocialNetwork _ _ m _) cid message = do
--     messages <- takeMVar m
--     let currChat = Map.lookup cid messages
--     case currChat of
--         Just chat -> do
--             let newChat = addToChat chat message
--             putMVar m (Map.insert cid newChat messages)
--         Nothing -> do
--             putMVar m (Map.insert cid (Chat [message]) messages)

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