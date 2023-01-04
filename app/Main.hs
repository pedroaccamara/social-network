module Main (main) where

-- import Lib
import Chat
import Thread


import Types -- for userThread
import Control.Concurrent -- for ThreadDelay -- for userThread


-- |The 'main' function starts the socialnetwork
main :: IO ()
main = do
    putStrLn "============================="
    putStrLn "Starting the Socialnetwork..."
    putStrLn "============================="
    sn <- initialiseSocialNetwork
    let numUsers = 2
    putStrLn $ "Working with " ++ show numUsers ++ " users"
    putStrLn "============================="
    let u :: User; u = User {
        userid = "0",
        username = "calvin_coolidge"
    }
    putStrLn "Waiting 2 secs"
    _ <- forkIO $ userThread sn u
    threadDelay (10^6 * 2)
    putStrLn "Secs gone"
    -- generateUserThreads sn numUsers
    -- createChat sn "01"
    -- sendMessage sn "01" "Heyyoooo"
    findChat sn "01" >>= (\x ->
        case x of
            Just chat -> do
                putStrLn $ show chat
            Nothing -> do
                print "Empty chat")
    -- findChat sn "01" >>= \case
    --         Just chat -> do print chat
    --         Nothing -> do print "Empty chat"
    -- sendMessage sn "02" "Heyyoooo 2"
    -- findChat sn "02" >>= print
