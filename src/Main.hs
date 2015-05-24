module Main where

import System.Environment
import Data.List

import Challenges

dispatch :: [(String, String)]
dispatch = [ ("1", challenge1) ]


main :: IO ()
main = do
    (com:args) <- getArgs
    let (Just f) = lookup com dispatch 
    putStr f
