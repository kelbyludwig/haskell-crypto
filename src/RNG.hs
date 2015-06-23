module RNG where

import Data.Bits as BIT
import System.Random as R
import Crypto as C
import Encoding as E
import Data.ByteString hiding (putStrLn, map, reverse, head, dropWhile)
import Data.ByteString.Lazy hiding (drop, putStrLn, map, reverse, head, dropWhile)
import Data.Time.Clock.POSIX
import Data.Word
import Data.Binary
import Data.Int

data MRNG = MRNG [Word32] Word32

initializeGenerator :: Word32 -> MRNG
initializeGenerator seed = MRNG (initializeGenerator' [seed'] 1) 0
                            where seed' = lower32 seed

initializeGenerator' :: [Word32] -> Word32 -> [Word32]
initializeGenerator' mt 623 = mt ++ [lower32 $ 1812433253 * (BIT.xor mtval $ BIT.shiftR mtval 30) + ind]
                                    where mtval = mt !! (fromIntegral (ind - 1) :: Int)
                                          ind = 623
initializeGenerator' mt ind = initializeGenerator' mt' (ind+1) 
                                      where mtval = mt !! (fromIntegral (ind - 1) :: Int)
                                            gend = lower32 $ 1812433253 * (BIT.xor mtval $ BIT.shiftR mtval 30) + ind
                                            mt' = mt ++ [gend]

extract_number :: MRNG -> (Word32, MRNG)
extract_number (MRNG s i) = case i == 0 of 
                                False -> (temper (s !! (fromIntegral i :: Int)), MRNG s (mod (i+1) 624))
                                True  -> let mrng' = generate_numbers (MRNG s i) in
                                           let (MRNG s' i') = mrng' in (temper (s' !! (fromIntegral i' :: Int)), (MRNG s' 1))

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
generate_numbers (MRNG state _) = MRNG (generate_numbers' state 0) 0


generate_numbers' :: [Word32] -> Int -> [Word32]                              
generate_numbers' state 624 = state
generate_numbers' state i   = generate_numbers' state' (i+1)
                               where mti    = state !! i
                                     mti'   = state !! (mod (i+1) 624)
                                     y      = (mti .&. 0x80000000) + (mti' .&. 0x7fffffff) 
                                     mti''  = state !! (mod (i+397) 624)
                                     y'     = BIT.xor mti'' (BIT.shiftR y 1)
                                     state' = if odd y then insert state y' i else insert state (BIT.xor y' 2567483615) i

insert :: [Word32] -> Word32 -> Int -> [Word32]
insert = \list item ind -> (Prelude.take ind list) ++ [item] ++ (Prelude.drop (ind+1) list)

takeBytes :: MRNG -> Int -> Data.ByteString.Lazy.ByteString 
takeBytes mrng num = Data.ByteString.Lazy.take (fromIntegral num :: Int64) $ Data.ByteString.Lazy.concat $ map encode nums 
                     where (ns,_) = takeRands mrng ((quot num 32) +1)
                           nums   = reverse ns
                           
takeRands :: MRNG -> Int -> ([Word32], MRNG)
takeRands mrng num = takeRands' mrng num [] 

takeRands' :: MRNG -> Int -> [Word32] -> ([Word32], MRNG)
takeRands' mrng 0 list = (list, mrng)
takeRands' mrng num list = let (x,m) = extract_number mrng in takeRands' m (num-1) (x:list)

lower32 :: Word32 -> Word32
lower32 = \x -> x .&. ((2^32)-1)

lower16 :: Word32 -> Word32
lower16 = \x -> x .&. ((2^16)-1)

timeSeedCrack :: Word32 -> Word32 -> Word32
timeSeedCrack cur expect = head $ dropWhile f (reverse [minn..cur])
                            where f = \x -> if let (r,_) = extract_number (initializeGenerator x) in r == expect then False else True
                                  minn = cur - 4000

challenge21 :: IO String
challenge21 = do
                let mrng = initializeGenerator 1
                let (ls,_) = takeRands mrng 10
                return $ show ls

challenge22 :: IO String
challenge22 = do
                r <- R.newStdGen
                let sleepy = head $ Prelude.take 1 (R.randomRs (40,1000) r) :: Int 
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
                let uls = reverse $ map untemper ls
                let mrngRec = MRNG uls 0
                putStrLn $ show $ let (x,_) = extract_number mrng' in x
                return $ show $   let (x,_) = extract_number mrngRec in x

convertBStoWord32 :: Data.ByteString.ByteString -> Word32
convertBStoWord32 x = decode (fromStrict x) :: Word32

--1. You know the number of random bytes because it increases
--   the known-plaintext length.
--2. Drop the number of random bytes + number of known bytes to 
--   closest 32 bit boundary.
--3. You can xor the known plaintext with the dropped ciphertext.
--   This will get you a number from the keystream.
--4. Since the key is only 16 bits, we can just bruteforce the key.
challenge24 :: IO String
challenge24 = do
                r <- R.newStdGen
                let rannum = head $ Prelude.take 1 (R.randomRs (0,100) r) :: Int
                let ranlet = Prelude.take rannum (R.randomRs ('A', 'Z') r)
                let ranbyt = E.toBytes ranlet
                let key = 5621
                return $ show "TODO: do challenge 24." 
