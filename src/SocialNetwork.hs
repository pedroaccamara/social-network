{-|
Module      : SocialNetwork
Description : This module includes all the SocialNetwork related functions
-}
module SocialNetwork (
    -- * Functions
    initialiseSocialNetwork,
    incrementSentMsgs,
    getSentMsgsCount,
) where


import Types (SocialNetwork(..))
import qualified Data.Map as Map
import Control.Concurrent (newMVar, takeMVar, putMVar)


-- |The 'initialiseSocialNetwork' function creates the map that will store all messages
initialiseSocialNetwork :: IO SocialNetwork
initialiseSocialNetwork = do
    d <- newMVar Map.empty
    m <- newMVar Map.empty
    return (SocialNetwork d m)

-- |The 'incrementSentMsgs' function increments the count of messages sent in the received socialnetwork, it should be called every time a message is sent
incrementSentMsgs :: SocialNetwork -> IO ()
incrementSentMsgs (SocialNetwork d _) = do
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

-- |The 'getSentMsgsCount' function returns the count of messages sent in the received socialnetwork
getSentMsgsCount :: SocialNetwork -> IO String
getSentMsgsCount (SocialNetwork d _) = do
    let key = "totalCount"
    database <- takeMVar d
    putMVar d database
    let totalCount = Map.lookup key database
    case totalCount of
        Just digit -> do
            return $ "A total of " ++ digit ++ " messages have been sent"
        Nothing -> do
            return "0 messages have been sent"