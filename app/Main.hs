module Main (main) where

-- import Lib
import Chat (findChat, displayChat)
import Thread (generateUserThreads)
import SocialNetwork (initialiseSocialNetwork, getSentMsgsCount, getUserIDReceivedMsgsCount)
import Types (SocialNetwork)

-- |The 'main' function starts the socialnetwork
main :: IO ()
main = do
    putStrLn "============================="
    putStrLn "Starting the Socialnetwork..."
    putStrLn "============================="
    let numUsers = 5 :: Int
    sn <- initialiseSocialNetwork numUsers
    putStrLn $ "Working with " ++ show numUsers ++ " users"
    putStrLn "============================="
    -- let u = User {
    --     userid = "0",
    --     username = "calvin_coolidge"
    -- }:: User
    -- _ <- forkIO $ userThread sn u
    generateUserThreads sn numUsers
    finalCount <- getSentMsgsCount sn
    putStrLn $ "A total of " ++ finalCount ++ " messages have been sent"
    loop sn

loop :: SocialNetwork -> IO ()
loop sn = do
    putStrLn "Want to see the chat between two particular users? E.g. 01? Write down their id's! (If you want to skip this, type Ctrl+C)"
    ids <- getLine :: IO String
    putStrLn $ "You inputted " ++ show (length ids) ++ " letters"
    if length ids == 2 then findChat sn ids >>= displayChat
    else do
        msgsReceived <- getUserIDReceivedMsgsCount sn ids
        putStrLn $ ids ++ " has received a total of " ++ msgsReceived
    loop sn