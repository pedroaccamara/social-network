{-|
Module      : Thread
Description : This module includes all the Thread related functions
-}
module Thread (
    -- * Functions
    userThread,
    generateUserThreads,
) where

import Types (SocialNetwork, User(..))
import Chat (sendMessage)
import SocialNetwork (incrementSentMsgs, chooseAnotherUser, incrementUserReceivedMsgs, getSentMsgsCount, startAtomicOp, finishAtomicOp)
import User (getRandomUsername)

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar, threadDelay)
import System.Random (randomIO)

-- |The 'userThread' function defines the IO () actions each user thread will perform
userThread :: SocialNetwork -> User -> IO ()
userThread sn u = loop (0 :: Int) :: IO ()
  where
    loop i = do
        putStrLn $ "At loop " ++ show i -- :: IO () -- REVISIT
        let limitMessages = 50
        messageReceiver <- chooseAnotherUser sn u
        putStrLn $ userid u ++ " sending a message to " ++ userid messageReceiver
        _ <- threadDelay <$> getDelay
        startAtomicOp sn
        total <- getSentMsgsCount sn
        -- if (read total :: Int) >= limitMessages then do putStrLn "Before msg"; return () -- REVISIT
        if (read total :: Int) >= limitMessages then do
            finishAtomicOp sn
            return ()
        else do
            sendMessage sn (userid u ++ userid messageReceiver) ("\n" ++ show u ++ ":\nMessage " ++ show i ++ "\n")
            incrementSentMsgs sn
            incrementUserReceivedMsgs sn messageReceiver
            finishAtomicOp sn
            newTotal <- getSentMsgsCount sn
            -- if (read newTotal :: Int) >= limitMessages then do putStrLn "After msg";return () -- REVISIT
            if (read newTotal :: Int) >= limitMessages then return ()
            else loop $ i + 1

-- |The 'generateUserThreads' function will generate however many user threads we need that will be responsible for sending a user's messages at random intervals -- REVISIT AFTER CREATEUSERS FUNC
generateUserThreads :: SocialNetwork -> Int -> IO ()
generateUserThreads sn n | n == 0 = return ()
    | otherwise = do
        randomUsername <- getRandomUsername
        let u = User {
            userid = show n,
            username = randomUsername
        } :: User
        done <- newEmptyMVar
        _ <- forkIO $ userThread sn u >> putMVar done ()
        generateUserThreads sn (n-1)
        takeMVar done

-- |The 'getDelay' function returns a random int representing a delay in microseconds between 0 and a half second
getDelay :: IO Int
getDelay = do
    let power = 6 :: Int
    let sec = 10^power
    num <- randomIO :: IO Float
    -- putStrLn $ "Waiting " ++ show (num*0.5) ++ " secs!" -- REVISIT
    return <$> round $ sec * num * 0.5