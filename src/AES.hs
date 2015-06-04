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
ecbChosenPrefix oracle = ecbChosenPrefix' oracle ctlen pad numblocks B.empty
                            where ct     = oracle B.empty
                                  ctlen  = B.length ct
                                  pad    = B.replicate ctlen 0
                                  numblocks = div ctlen 16

ecbChosenPrefix' :: (B.ByteString -> B.ByteString) -> Int -> B.ByteString -> Int -> B.ByteString -> B.ByteString
ecbChosenPrefix' _      0 _   _         _     = B.empty
ecbChosenPrefix' oracle n pad numblocks known = if pad == B.empty then B.empty else B.append byte (ecbChosenPrefix' oracle (n-1) (B.tail pad) numblocks (B.append known byte)) 
                                                 where letters    = map B.singleton [0..255]
                                                       filler     = B.append (B.tail pad) known
                                                       chosenpts  = map (B.append filler) letters
                                                       dropnum    = 16 * (numblocks - 1)
                                                       chosencts  = map (\x -> B.take 16 $ B.drop dropnum $ oracle x) chosenpts
                                                       unknown    = B.take 16 $ B.drop dropnum $ oracle (B.tail pad)
                                                       byte = case elemIndex unknown chosencts of
                                                                        (Just ind) -> letters !! ind
                                                                        Nothing    -> B.singleton 42                

ecbRandomPrefixDecrypt :: (B.ByteString -> B.ByteString) -> B.ByteString
ecbRandomPrefixDecrypt oracle = ecbChosenPrefix' oracle' (B.length secretPlusPrefixBytes) pad numberOfNULBlocks B.empty 
                                 where secretPlusPrefixBytes  = oracle B.empty
                                       soManyNULs             = iterate (B.append $ B.singleton 0) (B.singleton 0)
                                       repeatBlocks           = \x -> let b = C.createBlocks x 16 in ((length b) - (length $ nub b))+1
                                       numberOfSecretBytes    = B.length secretPlusPrefixBytes
                                       numberOfSecretBlocks   = div numberOfSecretBytes 16
                                       paddingLength          = 1 + (length $ takeWhile (/= numberOfSecretBlocks) $ map (\x -> repeatBlocks $ oracle x) soManyNULs)
                                       pad                    = B.replicate paddingLength 0
                                       blocks                 = C.createBlocks (oracle pad) 16
                                       dropPrefixBlocks       = B.concat $ concat $ dropPrefix blocks
                                       numberOfDroppedBytes   = (B.length $ oracle pad) - (B.length dropPrefixBlocks)
                                       numberOfNULBlocks      = repeatBlocks $ oracle pad
                                       oracle'                = \x -> B.drop numberOfDroppedBytes $ oracle x   
dropPrefix :: [B.ByteString] -> [[B.ByteString]]
dropPrefix list = dropWhile (\x -> length x == 1) $ group list                                                               

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
pkcs7 bs padsize = if padlen == 16 then bs else  B.append bs (B.replicate padlen pad)
                            where bslen = B.length bs
                                  padlen = padsize - (mod bslen padsize) 
                                  pad = (B.unpack $ B.singleton $ fromIntegral padlen) !! 0

createECBOracle :: B.ByteString -> IO (B.ByteString -> B.ByteString)
createECBOracle hidden = do 
                    g <- R.newStdGen
                    let key = B.pack $ Prelude.take 16 (R.randomRs (W8._nul, 255) g)
                    let oracle = (\input -> ecbEncrypt key $ pkcs7 (B.append input hidden) 16)
                    return oracle

createECBOracleRandomPrefix :: B.ByteString -> IO (B.ByteString -> B.ByteString)
createECBOracleRandomPrefix  hidden = do 
                                         g <- R.newStdGen
                                         let key = B.pack $ Prelude.take 16 (R.randomRs (W8._nul, 255) g)
                                         g2 <- R.newStdGen
                                         let len = head $ take 1 (R.randomRs (0, 64) g2)
                                         g3 <- R.newStdGen
                                         let prefix = B.pack $ take len (R.randomRs (W8._nul, 255) g3)
                                         let oracle = (\input -> ecbEncrypt key $ pkcs7 (B.concat [prefix, input, hidden]) 16)
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

createAESKey :: IO (B.ByteString)
createAESKey = do
                 g <- R.newStdGen
                 return $ B.pack $ take 16 (R.randomRs (W8._nul, 255) g)
