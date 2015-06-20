module RNG where

import Data.Bits as BIT

data MRNG = MRNG [Int] Int

initializeGenerator :: Int -> MRNG
initializeGenerator seed = MRNG (initializeGenerator' [seed'] 1) 0
                            where seed' = lower32 seed

initializeGenerator' :: [Int] -> Int -> [Int]
initializeGenerator' mt 623 = mt ++ [lower32 $ 1812433253 * (BIT.xor mtval $ BIT.shiftR mtval 30) + index]
                                    where mtval = mt !! (index - 1)
                                          index = 623
initializeGenerator' mt index = initializeGenerator' mt' (index+1) 
                                      where mtval = mt !! (index - 1)
                                            gend = lower32 $ 1812433253 * (BIT.xor mtval $ BIT.shiftR mtval 30) + index
                                            mt' = mt ++ [gend]

extract_number :: MRNG -> (Int, MRNG)
extract_number (MRNG s i) = case i == 0 of 
                                False -> (temper (s !! i), MRNG s (mod (i+1) 624))
                                True  -> let mrng' = generate_numbers (MRNG s i) in
                                           let (MRNG s' i') = mrng' in (temper (s' !! i'), mrng')

temper :: Int -> Int
temper i = op3 
            where op0 = BIT.xor i (BIT.shiftR i 11)
                  op1 = BIT.xor op0 $ 2636928640 .&. (BIT.shiftL op0 7)
                  op2 = BIT.xor op1 $ 4022730752 .&. (BIT.shiftL op1 15)
                  op3 = BIT.xor op2 $ BIT.shiftR op2 18

generate_numbers :: MRNG -> MRNG
generate_numbers (MRNG state _) = MRNG (generate_numbers' state state 0) 0

generate_numbers' :: [Int] -> [Int] -> Int -> [Int]                              
generate_numbers' (x:[]) s i = if mod y 2 == 0 then [y'] else [BIT.xor y' 2567483615] 
                                 where y  = (x .&. 0x80000000) + ((s !! (mod (i+1) 624)) .&. 0x7fffffff)
                                       y' = BIT.xor (BIT.shiftR y 1)  (s !! (mod (i+397) 624))
generate_numbers' (x:xs) s i = if mod y 2 == 0 then y' : generate_numbers' xs s (i+1) else (BIT.xor y' 2567483615) : generate_numbers' xs s (i+1) 
                                 where y  = (x .&. 0x80000000) + ((s !! (mod (i+1) 624)) .&. 0x7fffffff)
                                       y' = BIT.xor (BIT.shiftR y 1)  (s !! (mod (i+397) 624))
                                 

lower32 :: Int -> Int
lower32 = \x -> x .&. ((2^32)-1)

