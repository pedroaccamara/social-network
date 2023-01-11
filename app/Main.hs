{-|
Module      : Main
Description : This module is responsible for functions directly contributing to the Main flow of the app
-}
module Main (
    -- * Functions
    main,
    interaction,
    hl,
) where

import Chat (findChat, displayChat)
import Thread (generateUserThreads)
import SocialNetwork (initialiseSocialNetwork, getUser, getSentMsgsCount, getUserIDReceivedMsgsCount, finalOutput)
import Types (SocialNetwork)

-- |The 'main' function starts the socialnetwork
main :: IO ()
main = do
    hl
    putStrLn "Starting the Socialnetwork..."
    hl
    let numUsers = 10 :: Int
    sn <- initialiseSocialNetwork numUsers
    generateUserThreads sn numUsers
    finalCount <- getSentMsgsCount sn
    hl
    putStrLn $ "A total of " ++ finalCount ++ " messages have been sent"
    hl
    finalOutput sn numUsers
    hl
    interaction sn

-- |The 'interaction' function allows for the user to give the app certain inputs to further examine the state of the socialnetwork upon completion of a messaging cycle
interaction :: SocialNetwork -> IO ()
interaction sn = do
    putStrLn "\nWant to further examine the chat between two users? Input their id's! E.g. 12 (or 21) prompts the chat between user 1 and 2. (To exit, input X):"
    ids <- getLine :: IO String
    case ids of
        "X" -> do putStrLn "Thank you for overseeing the Socialnetwork App"
        [_] -> do
            user <- getUser sn ids
            msgsReceived <- getUserIDReceivedMsgsCount sn ids
            putStrLn $ show user ++ " has received a total of " ++ msgsReceived ++ " messages"
            interaction sn
        [suid1,suid2] -> do
            findChat sn [suid1] [suid2] >>= displayChat
            interaction sn
        [suid1,'1','0'] -> do
            findChat sn [suid1] "10" >>= displayChat
            interaction sn
        ['1','0',suid2] -> do
            findChat sn "10" [suid2] >>= displayChat
            interaction sn
        _ -> do
            putStrLn "That isn't an input I recognise, try one user id, or a pair of them"
            interaction sn

-- |The 'hl' function prints a horizontal line to separate outputs
hl :: IO ()
hl = putStrLn "============================="