module Main where

import System.Environment
import Challenges

dispatch :: [(String, IO String)]
dispatch = [ ("1", challenge1),
             ("2", challenge2),
             ("3", challenge3),
             ("4", challenge4),
             ("5", challenge5) ]


main :: IO ()
main = do
    (com:_) <- getArgs
    let (Just output) = lookup com dispatch 
    result <- output
    putStr result
