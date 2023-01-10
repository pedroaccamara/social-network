module Rand (
    main2,
) where

import Data.Text(Text, justifyLeft, justifyRight)
import Data.Text.IO as TI

myText :: Text
myText = "Hello stackoverflow."

main2 :: IO ()
main2 = do
    TI.putStrLn (justifyLeft 40 ' ' myText)
    TI.putStrLn (justifyRight 40 ' ' myText)