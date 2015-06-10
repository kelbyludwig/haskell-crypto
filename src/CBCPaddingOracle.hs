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
                return $ show $ paddingOracleAttack oracle ct 

--paddingOracleAttack :: (B.ByteString -> Bool) -> B.ByteString -> W8.Word8
paddingOracleAttack oracle ct = interBlock oracle ct (B.length ct) 
                        
interBlock oracle ct 0 = []       
interBlock oracle ct n = byte : interBlock oracle ct (n-1)
                         where blocks          = C.createBlocks ct 16
                               len             = length blocks
                               manipulateBlock = head $ drop (len-2) blocks
                               manipulated     = blockByteOptions manipulateBlock (n-1)
                               newCipherTexts  = map (\x -> insertBlock x (len-1) blocks) manipulated
                               mapped          = map oracle newCipherTexts 
                               (Just b)     = elemIndex True mapped
                               (Just byte) = lookup b intByteMap


paddingOracle :: B.ByteString -> B.ByteString -> B.ByteString -> Bool
paddingOracle key iv bs = case AES.pkcs7Strip pt of
                            Left  _ -> False
                            Right _ -> True
                          where pt = AES.cbcDecrypt key iv bs

insertByte :: B.ByteString -> Int -> W8.Word8 -> B.ByteString
insertByte bs index byte = B.pack $ concat [take index bytes, [byte],  drop (index+1) bytes]
                            where bytes = B.unpack bs

insertBlock :: B.ByteString -> Int -> [B.ByteString] -> B.ByteString
insertBlock b index bs = B.concat $ take (index-1) bs ++ [b] ++ (drop index bs)

--Generates all 255 options of a block with byte i modified (i is 0-indexed).
blockByteOptions :: B.ByteString -> Int -> [B.ByteString]
blockByteOptions bs index = map (insertByte bs index) bytes
                             where bytes = [W8._nul..] 

intByteMap :: [(Int, W8.Word8)]
intByteMap = zip [0..] [W8._nul..]


--We have c[n-1][16] ^ i[n-1][16] = p[n-1][16]
