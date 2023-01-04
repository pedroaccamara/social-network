{-|
Module      : Chat
Description : This module includes all the chat related functions
-}
module Chat (
    -- * Functions
    initialiseSocialNetwork,
    createChat,
    findChat,
    sendMessage,
    displayChat,
) where

import Types
import Control.Concurrent -- Chan, newChan, writeChan, readChan
import Data.Map (Map)
import qualified Data.Map as Map

-- |The 'initialiseSocialNetwork' function creates the map that will store all messages
initialiseSocialNetwork :: IO SocialNetwork
initialiseSocialNetwork = do
    m <- newMVar Map.empty
    return (SocialNetwork m)

-- |The 'createChat' function adds a chat to the map storing all messages
createChat :: SocialNetwork -> ChatID -> IO ()
createChat (SocialNetwork m) cid = do
    let newChat = Chat []
    messages <- takeMVar m
    putMVar m (Map.insert cid newChat messages)
    -- Potential improvement to avoid space leaks
    -- let messages' = Map.insert cid newChat messages
    -- putMVar m messages'
    -- seq messages' (return ())

-- |The 'findChat' function looks up for a chat with the given chatid as key
findChat :: SocialNetwork -> ChatID -> IO (Maybe Chat)
findChat (SocialNetwork m) cid = do
    messages <- takeMVar m
    putMVar m messages
    return (Map.lookup cid messages)

-- |The 'sendMessage' function adds a message to a chat between two users
sendMessage :: SocialNetwork -> ChatID -> Message -> IO ()
sendMessage (SocialNetwork m) cid message = do
    messages <- takeMVar m
    let currChat = Map.lookup cid messages
    case currChat of
        Just chat -> do
            -- let newChat = chat ++ [message]
            let newChat = addToChat chat message
            putMVar m (Map.insert cid newChat messages)
        Nothing -> do
            putMVar m (Map.insert cid (Chat [message]) messages)

-- |The 'displayChat' function takes a (Maybe Chat) and pretty prints it
displayChat :: Maybe Chat -> IO ()
displayChat x = case x of
    Just chat -> do print chat
    Nothing -> print "Empty chat"