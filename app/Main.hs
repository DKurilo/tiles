module Main where

import           Lib
import           System.IO

main :: IO ()
main = do
    b <- read <$> getContents
    print . solve $ b
