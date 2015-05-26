module Main where

import System.Environment

import Challenges

dispatch :: [(String, String)]
dispatch = [ ("1", challenge1),
             ("2", challenge2) ]


main :: IO ()
main = do
    (com:_) <- getArgs
    let (Just f) = lookup com dispatch 
    putStr f
