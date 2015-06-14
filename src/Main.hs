module Main where

import System.Environment
import Challenges
import CBCPaddingOracle
import CTR

dispatch :: [(String, IO String)]
dispatch = [ ("1", challenge1),
             ("2", challenge2),
             ("3", challenge3),
             ("4", challenge4),
             ("5", challenge5), 
             ("6", challenge6),
             ("7", challenge7),
             ("8", challenge8),
             ("9", challenge9),
             ("10", challenge10),
             ("11", challenge11),
             ("12", challenge12),
             ("13", challenge13),
             ("14", challenge14),
             ("15", challenge15),
             ("16", challenge16),
             ("17", challenge17), 
             ("18", challenge18) ]


main :: IO ()
main = do
    (com:_) <- getArgs
    let (Just output) = lookup com dispatch 
    result <- output
    putStr result
