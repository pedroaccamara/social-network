module Rand (
    main2,
) where

import Lib

main2 :: IO ()
main2 = do
    username <- getRandomUsername
    putStrLn username