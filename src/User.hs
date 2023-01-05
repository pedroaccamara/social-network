{-|
Module      : User
Description : This module includes all the User related functions
-}
module User (
    -- * Functions
    getRandomUsername,
) where

import System.Random (randomRIO)

-- |The 'getRandomUsername' function chooses a random name and creates a username with it
getRandomUsername :: IO String
getRandomUsername = do createUserName <$> getRandomName

-- |The 'getRandomName' function randomly chooses from the list of available names
getRandomName :: IO String
getRandomName = do
    allNames <- readFile "data/names.txt"
    nameIndex <- randomRIO (0,44) :: IO Int
    return $ lines allNames !! nameIndex

-- |The 'createUserName' takes a full name and joins its names with an underscore suitable for a socialnetwork's username
createUserName :: String -> String
createUserName "" = ""
createUserName fullname = let names = words fullname in head names ++ concatMap ("_" ++) (tail names)