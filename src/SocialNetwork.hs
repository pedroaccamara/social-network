{-|
Module      : SocialNetwork
Description : This module includes all the SocialNetwork related functions
-}
module SocialNetwork (
    -- * Functions
    initialiseSocialNetwork,
    getUser,
    incrementSentMsgs,
    getSentMsgsCount,
    chooseAnotherUser,
    incrementUserReceivedMsgs,
    getUserIDReceivedMsgsCount,
    finalOutput,
    startAtomicOp,
    finishAtomicOp,
) where

import Types (SocialNetwork(..), UserID, User(..), Userbase, Key, Value)
import User (getRandomUsername)

import qualified Data.Map as Map
import Control.Concurrent (MVar, newEmptyMVar, newMVar, takeMVar, putMVar)
import System.Random (randomRIO)

-- |The 'initialiseSocialNetwork' function creates the map that will store all messages
initialiseSocialNetwork :: Int -> IO SocialNetwork
initialiseSocialNetwork n = do
    d <- newMVar Map.empty
    u <- newMVar Map.empty
    m <- newMVar Map.empty
    atomic <- newEmptyMVar
    createUsers u n
    return (SocialNetwork d u m atomic)

-- |The 'startAtomicOp' function allows for a thread to know it is the only thread performing an action on the socialnetwork that other threads can't perform at the same time
startAtomicOp :: SocialNetwork -> IO ()
startAtomicOp (SocialNetwork _ _ _ atomic) = putMVar atomic ()

-- |The 'finishAtomicOp' function frees up the socialnetwork so that other threads can perform an operation that can't be performed by multiple threads at once
finishAtomicOp :: SocialNetwork -> IO ()
finishAtomicOp (SocialNetwork _ _ _ atomic) = takeMVar atomic

-- |The 'createUsers' functions will register a chosen number of users in a socialnetwork's userbase
createUsers :: MVar Userbase -> Int -> IO ()
createUsers u n | n == 0 = return ()
    | otherwise = do
        randomUsername <- getRandomUsername
        let newUser = User {
            userid = show n,
            username = randomUsername
        } :: User
        userbase <- takeMVar u
        putMVar u (Map.insert (userid newUser) newUser userbase)
        createUsers u (n-1)

-- |The 'getNumberOfUsers' function returns the number of users registered in the socialnetwork
getNumberOfUsers :: SocialNetwork -> IO Int
getNumberOfUsers (SocialNetwork _ u _ _) = do
    userbase <- takeMVar u
    putMVar u userbase
    return $ Map.size userbase
    -- let numUsers = Map.size userbase
    -- return numUsers -- REVISIT

-- |The 'getUser' function returns the socialnetwork user with the given userid
getUser :: SocialNetwork -> UserID -> IO User
getUser (SocialNetwork _ u _ _) uid = do
    userbase <- takeMVar u
    putMVar u userbase
    return $ userbase Map.! uid

-- |The 'chooseAnotherUser' function randomly chooses a user different to the one it receives
chooseAnotherUser :: SocialNetwork -> User -> IO User
chooseAnotherUser sn chooser = do
    numUsers <- getNumberOfUsers sn
    let uid = read $ userid chooser :: Int
    let poolChoice = init [1..uid] ++ tail [uid..numUsers]
    randNum <- randomRIO (0, length poolChoice - 1) :: IO Int
    let otherUID = show $ poolChoice!!randNum
    getUser sn otherUID

-- |The 'databaseValueFromKey' function returns the lookup result of a key in the socialnetwork's database
databaseValueFromKey :: SocialNetwork -> Key -> IO (Maybe Value)
databaseValueFromKey (SocialNetwork d _ _ _) key = do
    database <- takeMVar d
    putMVar d database
    let value = Map.lookup key database
    return value

-- |The 'databaseNumValueFromKey' function expects to look for a number in the socialnetwork's database meaning it can return a 0 in case it doesn't find a value associated with the given key
databaseNumValueFromKey :: SocialNetwork -> Key -> IO String
databaseNumValueFromKey sn key = do
    numValue <- databaseValueFromKey sn key
    case numValue of
        Just digit -> do
            return digit
        Nothing -> do
            return "0"

-- |The 'incrementSentMsgs' function increments the count of messages sent in the received socialnetwork, it should be called every time a message is sent
incrementSentMsgs :: SocialNetwork -> IO ()
incrementSentMsgs (SocialNetwork d _ _ _) = do
    let key = "totalCount"
    database <- takeMVar d
    let totalCount = Map.lookup key database
    case totalCount of
        Just digit -> do
            let parsed = read digit :: Int
            let newTotal = parsed + 1
            if newTotal `rem` 5 == 0 then do print $ "Reached " ++ show newTotal ++ " messages"
            else do putStr ""
            putMVar d (Map.insert key (show newTotal) database)
        Nothing -> do
            putMVar d (Map.insert key "1" database)

-- |The 'getSentMsgsCount' function returns the count of messages sent in the socialnetwork
getSentMsgsCount :: SocialNetwork -> IO String
getSentMsgsCount sn = do databaseNumValueFromKey sn "totalCount"

-- |The 'incrementUserReceivedMsgs' function increments the count of received messages this user has in the socialnetwork's database
incrementUserReceivedMsgs :: SocialNetwork -> User -> IO ()
incrementUserReceivedMsgs (SocialNetwork d _ _ _) u = do
    let key = userid u
    database <- takeMVar d
    let receivedMsgCount = Map.lookup key database
    case receivedMsgCount of
        Just digit -> do
            let parsed = read digit :: Int
            let newCount = parsed + 1
            putMVar d (Map.insert key (show newCount) database)
        Nothing -> do
            putMVar d (Map.insert key "1" database)

-- |The 'getUserReceivedMsgsCount' function returns the count of messages this user has received in the socialnetwork
getUserReceivedMsgsCount :: SocialNetwork -> User -> IO String
getUserReceivedMsgsCount sn u = getUserIDReceivedMsgsCount sn $ userid u

-- |The 'getUserIDReceivedMsgsCount' function returns the count of messages the user with this userid has received in the socialnetwork
getUserIDReceivedMsgsCount :: SocialNetwork -> UserID -> IO String
getUserIDReceivedMsgsCount sn uid = do databaseNumValueFromKey sn uid

-- |The 'finalOutput' function outputs the final count of how many messages each user received 
finalOutput :: SocialNetwork -> Int -> IO ()
finalOutput _ 0 = return ()
finalOutput sn n = do
    finalOutput sn $ n-1
    user <- getUser sn $ show n
    receivedMsgCount <- getUserReceivedMsgsCount sn user
    putStrLn $ show user ++ " has received " ++ receivedMsgCount ++ " messages"