{-|
Module      : User
Description : This module includes all the User related functions
-}
module User (
    -- * Functions
    getRandomUsername,
    getUids,
    getSuids,
    earlierUser,
    earlierSuid,
) where

import Types (User(..))

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

-- |The 'getSuids' function receives two users and returns their user ids as strings
getSuids :: (User, User) -> (String, String)
getSuids (u1, u2) = do
    (userid u1, userid u2)

-- |The 'getUids' function receives two users and returns their user ids as ints
getUids :: (User, User) -> (Int, Int)
getUids (u1, u2) = do
    let uid1 = userid u1
    let uid2 = userid u2
    (read uid1 :: Int, read uid2 :: Int)

-- |The 'earlierUser' function receives two users and returns whether the first user given as argument has a smaller user id than the second user
earlierUser :: User -> User -> Bool
earlierUser u1 u2 = do
    let (uid1, uid2) = getUids (u1,u2)
    uid1 < uid2

-- |The 'earlierSuid' function receives two user ids as string and returns whether the first user id given as argument is smaller than the second user id
earlierSuid :: String -> String -> Bool
earlierSuid suid1 suid2 = (read suid1 :: Int) < (read suid2 :: Int)