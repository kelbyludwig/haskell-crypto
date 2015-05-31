module AES where

import Data.List
import qualified Data.Word8 as W8
import qualified System.Random as R
import qualified Crypto.Cipher.AES as AES
import qualified Data.ByteString as B
import qualified Crypto as C


data AESMode = ECB | CBC deriving (Show)

--ECB related functions
ecbEncrypt :: B.ByteString -> B.ByteString -> B.ByteString
ecbEncrypt key mes = AES.encryptECB (AES.initAES key) mes

ecbDecrypt :: B.ByteString -> B.ByteString -> B.ByteString
ecbDecrypt key ct = AES.decryptECB (AES.initAES key) ct

ecbDetect :: B.ByteString -> Bool
ecbDetect bs = if length blocks == length blocksNoDupes then False else True
                where blocks        = C.createBlocks bs 16
                      blocksNoDupes = nub blocks

ecbChosenPrefix :: (B.ByteString -> B.ByteString) -> B.ByteString
ecbChosenPrefix oracle = ecbChosenPrefix' oracle 16 B.empty
                            where ct  = B.length $ oracle B.empty
                                  max = div ct 16

ecbChosenPrefix' :: (B.ByteString -> B.ByteString) -> Int -> B.ByteString -> B.ByteString
ecbChosenPrefix' _      0 known = known
ecbChosenPrefix' oracle 0 known = B.append known (ecbChosenPrefix' oracle 16 B.empty)
ecbChosenPrefix' oracle n known = B.append byte  (ecbChosenPrefix' oracle (n-1) (B.append known byte)) 
                                         where letters   = map B.singleton $ filter W8.isPrint [0..255]
                                               filler    = B.replicate (n-1) 0
                                               chosenpts = map (B.append (B.append filler known)) letters
                                               chosencts = map (\x -> B.take 16 $ oracle x) chosenpts
                                               unknown   = B.take 16 $ oracle filler
                                               (Just index) = elemIndex unknown chosencts
                                               byte = letters !! index

--CBC related functions
cbcEncrypt :: B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString
cbcEncrypt key iv mes = cbcEncrypt' aes blocks iv
                            where blocks = C.createBlocks mes 16
                                  aes = ecbEncrypt key 

cbcEncrypt' :: (B.ByteString -> B.ByteString) -> [B.ByteString] -> B.ByteString -> B.ByteString
cbcEncrypt' _ [] _     = B.empty
cbcEncrypt' aes (m:[]) ct = aes (C.xor' m ct)
cbcEncrypt' aes (m:ms) ct = B.append ct' (cbcEncrypt' aes ms ct')
                                        where ct' = aes (C.xor' m ct)

cbcDecrypt :: B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString
cbcDecrypt key iv ct = cbcDecrypt' aes blocks iv
                            where blocks = C.createBlocks ct 16
                                  aes = ecbDecrypt key

cbcDecrypt' :: (B.ByteString -> B.ByteString) -> [B.ByteString] -> B.ByteString -> B.ByteString
cbcDecrypt'  _ [] _ = B.empty
cbcDecrypt' aes (c:[]) m = C.xor' (aes c) m
cbcDecrypt' aes (c:cs) m  = B.append m' (cbcDecrypt' aes cs c)
                                    where m' = C.xor' (aes c) m

cbcOrEbc :: (B.ByteString -> B.ByteString) -> AESMode
cbcOrEbc oracle = if ecbDetect ciphert then ECB else CBC
                    where mybytes = B.replicate 200 0      
                          ciphert = oracle mybytes

--Misc. AES Functions
pkcs7 :: B.ByteString -> Int -> B.ByteString
pkcs7 bs padsize = B.append bs (B.replicate padlen pad)
                            where bslen = B.length bs
                                  padlen = padsize - (mod bslen padsize) 
                                  pad = (B.unpack $ B.singleton $ fromIntegral padlen) !! 0

createECBOracle :: B.ByteString -> IO (B.ByteString -> B.ByteString)
createECBOracle hidden = do 
                    g <- R.newStdGen
                    let key = B.pack $ Prelude.take 16 (R.randomRs (W8._nul, 255) g)
                    let oracle = (\input -> ecbEncrypt key $ pkcs7 (B.append input hidden) 16)
                    return oracle

createAESOracle :: IO (AESMode, (B.ByteString -> B.ByteString))
createAESOracle = do
                    g <- R.newStdGen
                    let coin = ((Prelude.take 1 (R.randomRs (0, 1) g)) :: [Int]) !! 0
                    g2 <- R.newStdGen
                    let lenPrepend = Prelude.take 1 (R.randomRs (5, 10) g2)
                    g3 <- R.newStdGen
                    let lenAppend  = Prelude.take 1 (R.randomRs (5, 10) g3)
                    g4 <- R.newStdGen
                    let bytesPre = B.pack $ Prelude.take (Prelude.sum lenPrepend) (R.randomRs (W8._nul, 255) g4)
                    g5 <- R.newStdGen
                    let bytesApp = B.pack $ Prelude.take (Prelude.sum lenAppend)  (R.randomRs (W8._nul, 255) g5)
                    g6 <- R.newStdGen
                    let key = B.pack $ Prelude.take 16 (R.randomRs (W8._nul, 255) g6)
                    let iv = B.pack $ Prelude.take 16 (drop 16  (R.randomRs (W8._nul, 255) g6))
                    let mode = if coin == 1 then cbcEncrypt key iv else ecbEncrypt key
                    let m = if coin == 1 then CBC else ECB
                    let oracle = (\input -> mode $ pkcs7 (B.append (B.append bytesPre input) bytesApp) 16)
                    return (m,oracle)
