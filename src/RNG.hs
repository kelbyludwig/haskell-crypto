module RNG where

import Data.Bits as BIT

initializeGenerator :: Int -> [Int]
initializeGenerator seed = initializeGenerator' [seed'] 1
                            where seed' = lower32 seed

initializeGenerator' :: [Int] -> Int -> [Int]
initializeGenerator' mt 623 = mt ++ [lower32 $ 1812433253 * (BIT.xor mtval $ BIT.shiftR mtval 30) + index]
                                    where mtval = mt !! (index - 1)
                                          index = 623
initializeGenerator' mt index = initializeGenerator' mt' (index+1) 
                                      where mtval = mt !! (index - 1)
                                            gend = lower32 $ 1812433253 * (BIT.xor mtval $ BIT.shiftR mtval 30) + index
                                            mt' = mt ++ [gend]

lower32 :: Int -> Int
lower32 = \x -> x .&. ((2^32)-1)

challenge21 = undefined
