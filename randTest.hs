module Rand (
    main2,
) where

import User

main2 :: IO ()
main2 = do
    username <- getRandomUsername
    putStrLn username