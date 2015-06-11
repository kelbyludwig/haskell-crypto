module CBCPaddingOracle where

import qualified Encoding as E
import qualified System.Random as R
import qualified AES as AES
import qualified Crypto as C
import qualified Data.ByteString as B
import qualified Data.Word8 as W8
import qualified Data.Bits as BIT

import Data.List


challenge17 :: IO String
challenge17 = do
                f <- readFile "./src/Files/17.txt"
                let ls = map E.fromBase64 $ lines f
                g <- R.newStdGen 
                let i = head $ take 1 (R.randomRs (0,9) g)
                let line = ls !! i
                key <- AES.createAESKey
                iv  <- AES.createAESKey
                let ct = AES.cbcEncrypt key iv $ AES.pkcs7 line 16
                let oracle = paddingOracle key iv             
                return $ show $ attack oracle iv ct

attack oracle iv ct = getIntermiedateState oracle test2 test 16 [] soh 
                       where blocks = C.createBlocks ct 16
                             len = length blocks
                             test = blocks !! (len-1)
                             test2 = blocks !! (len-2)
                             soh = [W8._nul..] !! 1
                             

--Should discover intermiedate state of dblock by manipulating rblock and sending to oracle
getIntermiedateState o r dblock 0 is padbyte = B.pack pt
                                                    where bs = B.unpack r
                                                          pt = zipWith BIT.xor bs is
getIntermiedateState o r dblock n is padbyte = getIntermiedateState o r dblock (n-1) is' padbyte'
                                                      where rblock   = B.replicate (n-1) 0
                                                            rblocks  = map (B.snoc rblock) [W8._nul..]
                                                            post     = B.pack $ map (\x -> BIT.xor x padbyte) is
                                                            newcts   = map (\x -> B.append (B.append x post) dblock) rblocks
                                                            mapo     = map o newcts
                                                            (i:[])   = elemIndices True mapo --Should fail (ungracefully) if no or more than one byte returns true from the oracle
                                                            (Just b) = lookup i intByteMap --Gives us CS[x]   
                                                            isbyte   = BIT.xor b padbyte   --Gives us IS[x]                                            
                                                            padbyte' = succ padbyte
                                                            is'      = isbyte : is
    --NEED: variable to keep track of is bytes.
    --NEED: to keep track of expected padding bytes.
    --NEED: to update ciphertext on each call.






--paddingOracleAttack :: (B.ByteString -> Bool) -> B.ByteString -> W8.Word8
--paddingOracleAttack oracle ct = interBlock oracle ct (B.length ct) 
                       
--attackBlock ct mb b o n = 
--                           where opts = blockByteOptions manipulateBlock n
--                                 cts  = map (\x -> insertBlock x (
 
--interBlock oracle ct 0 b = []       
--interBlock oracle ct n b = byte' : interBlock oracle ct' (n-1) (succ b)
--                                  where blocks          = C.createBlocks ct 16
--                                        len             = length blocks
--                                        index           = ((len-2) * 16) + (n-1)
--                                        newcts          = newCiphertexts blocks n
--                                        mapped          = map oracle newcts 
--                                        (Just bi)       = elemIndex True mapped
--                                        (Just byte)     = lookup bi intByteMap
--                                        byte'           = BIT.xor byte b
--                                        ct' = insertByte ct index (BIT.xor byte' (succ b))
--
paddingOracle :: B.ByteString -> B.ByteString -> B.ByteString -> Bool
paddingOracle key iv bs = pkcs7Valid pt
                          where pt = AES.cbcDecrypt key iv bs

insertByte :: B.ByteString -> Int -> W8.Word8 -> B.ByteString
insertByte bs index byte = if index >= B.length bs then error "Insert index out of range" else B.pack $ concat [take index bytes, [byte],  drop (index+1) bytes]
                            where bytes = B.unpack bs

insertBlock :: B.ByteString -> Int -> [B.ByteString] -> B.ByteString
insertBlock b index bs = B.concat $ take (index-1) bs ++ [b] ++ (drop index bs)

--Generates all 255 options of a block with byte i modified (i is 0-indexed).
blockByteOptions :: B.ByteString -> Int -> [B.ByteString]
blockByteOptions bs index = map (insertByte bs index) bytes
                             where bytes = [W8._nul..] 

intByteMap :: [(Int, W8.Word8)]
intByteMap = zip [0..] [W8._nul..]

byteIntMap :: [(W8.Word8, Int)]
byteIntMap = zip [W8._nul..] [0..]

newCiphertexts :: [B.ByteString] -> Int -> [B.ByteString]
newCiphertexts bs index = map (\x -> insertBlock x (len-1) bs) opts
                            where len  = length bs
                                  mb   = head $ drop (len-2) bs
                                  opts = blockByteOptions mb (index-1)

pkcs7Valid bs = if length nubbed == 1 then True else False 
                  where padbyte    = B.last bs
                        (Just int) = lookup padbyte byteIntMap
                        len        = B.length bs
                        padding    = B.drop (len - int) bs
                        nubbed     = B.group padding   
