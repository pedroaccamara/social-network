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
import Control.Concurrent (forkIO)
import SocialNetwork (incrementSentMsgs)
import User (getRandomUsername)

-- |The 'userThread' function defines the IO () actions each user thread will perform
userThread :: SocialNetwork -> User -> IO ()
userThread sn u = loop (0 :: Int) :: IO ()
  where
    loop i = do
        putStrLn $ "At loop " ++ show i -- :: IO () -- REVISIT
        if i > 3
            then return ()
        else do sendMessage sn (userid u ++ "1") ("\n" ++ show u ++ ":\nMessage " ++ show i ++ "\n")
                -- putStrLn $ show u ++ " incrementing msgs at " ++ show (i+1) -- REVISIT
                incrementSentMsgs sn
                loop $ i + 1
            -- :: IO () -- REVISIT
        -- :: IO () -- REVISIT

-- |The 'generateUserThreads' function will generate however many user threads we need that will be responsible for sending a user's messages at random intervals
generateUserThreads :: SocialNetwork -> Int -> IO ()
generateUserThreads sn n | n == -1 = return ()
    | otherwise = do
        randomUsername <- getRandomUsername
        let u = User {
            userid = show n,
            username = randomUsername
        } :: User
        _ <- forkIO $ userThread sn u
        generateUserThreads sn (n-1)

    -- let sec = 10^power -- REVISIT
    -- threadDelay (sec * 2)
    -- putStrLn "Secs gone"
    -- -- generateUserThreads sn numUsers
    -- -- createChat sn "01"
    -- -- sendMessage sn "01" "Heyyoooo"
    -- findChat sn "01" >>= displayChat