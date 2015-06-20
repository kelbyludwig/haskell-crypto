module RNG where

import Data.Bits as BIT
import System.Random as R
import Data.Time.Clock.POSIX
import Control.Concurrent
import Data.Word

data MRNG = MRNG [Word32] Word32

initializeGenerator :: Word32 -> MRNG
initializeGenerator seed = MRNG (initializeGenerator' [seed'] 1) 0
                            where seed' = lower32 seed

initializeGenerator' :: [Word32] -> Word32 -> [Word32]
initializeGenerator' mt 623 = mt ++ [lower32 $ 1812433253 * (BIT.xor mtval $ BIT.shiftR mtval 30) + index]
                                    where mtval = mt !! (fromIntegral (index - 1) :: Int)
                                          index = 623
initializeGenerator' mt index = initializeGenerator' mt' (index+1) 
                                      where mtval = mt !! (fromIntegral (index - 1) :: Int)
                                            gend = lower32 $ 1812433253 * (BIT.xor mtval $ BIT.shiftR mtval 30) + index
                                            mt' = mt ++ [gend]

extract_number :: MRNG -> (Word32, MRNG)
extract_number (MRNG s i) = case i == 0 of 
                                False -> (temper (s !! (fromIntegral i :: Int)), MRNG s (mod (i+1) 624))
                                True  -> let mrng' = generate_numbers (MRNG s i) in
                                           let (MRNG s' i') = mrng' in (temper (s' !! (fromIntegral i' :: Int)), mrng')

temper :: Word32 -> Word32
temper i = op3
            where op0 = BIT.xor i (BIT.shiftR i 11)
                  op1 = BIT.xor op0 $ 2636928640 .&. (BIT.shiftL op0 7)
                  op2 = BIT.xor op1 $ 4022730752 .&. (BIT.shiftL op1 15)
                  op3 = BIT.xor op2 $ BIT.shiftR op2 18

untemper :: Word32 -> Word32
untemper i = op3
             where op0 = BIT.xor i (BIT.shiftR i 18)
                   op1 = BIT.xor op0 $ 4022730752 .&. (BIT.shiftL op0 15)
                   op2 = BIT.xor op1 ((sf $ sf $ sf $ sf $ BIT.shiftL op1 7) .&. 2636928640)
                   sf  = \x -> BIT.shiftL (BIT.xor op1 (x .&. 2636928640)) 7
                   op3 = BIT.xor op2 $ BIT.shiftR (BIT.xor op2 (BIT.shiftR op2 11)) 11
            

generate_numbers :: MRNG -> MRNG
generate_numbers (MRNG state _) = MRNG (generate_numbers' state state 0) 0

generate_numbers' :: [Word32] -> [Word32] -> Int -> [Word32]                              
generate_numbers' (x:[]) s i = s'
                                 where y  = (x .&. 0x80000000) + ((s !! (fromIntegral (mod (i+1) 624) :: Int)) .&. 0x7fffffff)
                                       y' = BIT.xor (BIT.shiftR y 1)  (s !! (fromIntegral (mod (i+397) 624) :: Int))
                                       s' = if mod y 2 == 0 then insert s y' i else insert s (BIT.xor y' 2567483615) i
generate_numbers' (x:xs) s i = generate_numbers' xs s' (i+1)
                                 where y  = (x .&. 0x80000000) + ((s !! (fromIntegral (mod (i+1) 624) :: Int)) .&. 0x7fffffff)
                                       y' = BIT.xor (BIT.shiftR y 1)  (s !! (fromIntegral (mod (i+397) 624) :: Int))
                                       s' = if mod y 2 == 0 then insert s y' i else insert s (BIT.xor y' 2567483615) i

insert :: [Word32] -> Word32 -> Int -> [Word32]
insert = \list item index -> (take index list) ++ [item] ++ (drop (index+1) list)

takeRands :: MRNG -> Int -> ([Word32], MRNG)
takeRands mrng num = takeRands' mrng num [] 

takeRands' :: MRNG -> Int -> [Word32] -> ([Word32], MRNG)
takeRands' mrng 0 list = (list, mrng)
takeRands' mrng num list = let (x,m) = extract_number mrng in takeRands' m (num-1) (x:list)

lower32 :: Word32 -> Word32
lower32 = \x -> x .&. ((2^32)-1)

timeSeedCrack :: Word32 -> Word32 -> Word32
timeSeedCrack cur expect = head $ dropWhile f (reverse [min..cur])
                            where f = \x -> if let (r,_) = extract_number (initializeGenerator x) in r == expect then False else True
                                  min = cur - 4000

challenge21 :: IO String
challenge21 = do
                let mrng = initializeGenerator 1
                let (ls,_) = takeRands mrng 10
                return $ show ls

challenge22 :: IO String
challenge22 = do
                r <- R.newStdGen
                let sleepy = head $ take 1 (R.randomRs (40,1000) r) :: Int 
                seed <- round `fmap` getPOSIXTime
                let seed' = fromIntegral (seed + sleepy) :: Word32
                putStrLn $ "The seed is: " ++ (show seed')
                let mrng = initializeGenerator seed'
                let (ran,_) = extract_number mrng
                cur <- round `fmap` getPOSIXTime
                putStrLn $ "The number recieved was: " ++ (show ran)
                return $ show $ timeSeedCrack (cur+2000) ran

challenge23 :: IO String
challenge23 = do
                let mrng = initializeGenerator 1
                let (ls, mrng') = takeRands mrng 624
                let (MRNG s i) = mrng'
                putStrLn $ "Normal state: " ++ (show $ take 5 s)
                putStrLn $ "Normal index: " ++ (show i)
                putStrLn $ "Normal rands: " ++ (show $ take 5 ls)
                let uls = map untemper ls
                putStrLn $ "Untempered state: " ++ (show $ take 5 uls)
                let mrngRec = MRNG uls 0
                putStrLn $ show $ let (x,_) = extract_number mrng' in x
                return $ show $   let (x,_) = extract_number mrngRec in x
