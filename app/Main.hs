module Main (main) where

import Lib
import Chat

-- |The 'main' function starts the socialnetwork
main :: IO ()
main = do
    putStrLn "============================="
    putStrLn "Starting the Socialnetwork..."
    putStrLn "============================="
    sn <- initialiseSocialNetwork
    putStrLn " Working with 2 users"
    putStrLn "============================="
    createChat sn "01"
    sendMessage sn "01" "Heyyoooo"
    findChat sn "01" >>= print
    sendMessage sn "02" "Heyyoooo 2"
    findChat sn "02" >>= print