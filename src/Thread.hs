{-|
Module      : Thread
Description : This module includes all the Thread related functions
-}
module Thread (
    -- * Functions
    userThread,
) where

import Types
import Chat

-- |The 'userThread' function defines the IO () actions each user thread will perform
userThread :: SocialNetwork -> User -> IO ()
userThread sn u = loop 0
  where
    loop i = do
        putStrLn $ "At loop " ++ show i
        if i > 3
            then return ()
        else do sendMessage sn (userid u ++ "1") ("\n" ++ show u ++ ":\nMessage " ++ show i ++ "\n")
                loop $ i + 1


-- |The 'generateUserThreads' function will generate however many user threads we need that will be responsible for sending a user's messages at random intervals
-- generateUserThreads :: SocialNetwork -> Int -> IO ()
-- generateUserThreads sn n = do
--     forkIO
