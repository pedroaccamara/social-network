module Main (main) where

-- import Lib
import Chat (findChat, displayChat)
import Thread (generateUserThreads)
import SocialNetwork (initialiseSocialNetwork, getSentMsgsCount)


-- import Types -- for userThread
import Control.Concurrent -- for ThreadDelay -- for userThread


-- |The 'main' function starts the socialnetwork
main :: IO ()
main = do
    putStrLn "============================="
    putStrLn "Starting the Socialnetwork..."
    putStrLn "============================="
    sn <- initialiseSocialNetwork
    let numUsers = 1 :: Int
    putStrLn $ "Working with " ++ show numUsers ++ " users"
    putStrLn "============================="
    -- let u = User {
    --     userid = "0",
    --     username = "calvin_coolidge"
    -- }:: User
    -- putStrLn "Waiting 2 secs"
    -- _ <- forkIO $ userThread sn u
    generateUserThreads sn numUsers
    let power = 6 :: Int
    let sec = 10^power
    threadDelay (sec * 2)
    putStrLn "Secs gone"
    -- generateUserThreads sn numUsers
    -- createChat sn "01"
    -- sendMessage sn "01" "Heyyoooo"
    findChat sn "01" >>= displayChat
    findChat sn "11" >>= displayChat
    finalCount <- getSentMsgsCount sn
    print finalCount
    -- sendMessage sn "02" "Heyyoooo 2"
    -- findChat sn "02" >>= print
