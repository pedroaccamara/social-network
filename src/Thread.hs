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
import Chat (sendMessage, getRandomMessage)
import SocialNetwork (getUser, incrementSentMsgs, chooseAnotherUser, incrementUserReceivedMsgs, getSentMsgsCount, startAtomicOp, finishAtomicOp)

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar, threadDelay)
import System.Random (randomIO)

-- |The 'userThread' function defines the IO () actions each user thread will perform
userThread :: SocialNetwork -> User -> IO ()
userThread sn u = loop (0 :: Int) :: IO ()
  where
    loop i = do
        let limitMessages = 100
        messageReceiver <- chooseAnotherUser sn u
        del <- getDelay
        threadDelay del
        -- _ <- threadDelay <$> getDelay -- REVISIT
        startAtomicOp sn
        total <- getSentMsgsCount sn
        if (read total :: Int) >= limitMessages then do
            finishAtomicOp sn
            return ()
        else do
            randomMessage <- getRandomMessage
            sendMessage sn u messageReceiver randomMessage
            incrementSentMsgs sn
            incrementUserReceivedMsgs sn messageReceiver
            finishAtomicOp sn
            newTotal <- getSentMsgsCount sn
            if (read newTotal :: Int) >= limitMessages then return ()
            else loop $ i + 1

-- |The 'generateUserThreads' function will generate however many user threads we need that will be responsible for sending a user's messages at random intervals -- REVISIT AFTER CREATEUSERS FUNC
generateUserThreads :: SocialNetwork -> Int -> IO ()
generateUserThreads sn n | n == 0 = return ()
    | otherwise = do
        u <- getUser sn $ show n
        done <- newEmptyMVar -- The done MVar will help blocking the main thread until all userThreads are "done"
        _ <- forkIO $ userThread sn u >> putMVar done ()
        generateUserThreads sn (n-1)
        takeMVar done

-- |The 'getDelay' function returns a random int representing a delay in microseconds between 0 and a half second
getDelay :: IO Int
getDelay = do
    let power = 6 :: Int
    let sec = 10^power
    num <- randomIO :: IO Float
    return <$> round $ sec * num * 0.5