module AES where

import Data.List
import qualified Crypto.Cipher.AES as AES
import qualified Data.ByteString as B
import qualified Crypto as C

--ECB related functions
ecbEncrypt :: B.ByteString -> B.ByteString -> B.ByteString
ecbEncrypt key mes = AES.encryptECB (AES.initAES key) mes

ecbDecrypt :: B.ByteString -> B.ByteString -> B.ByteString
ecbDecrypt key ct = AES.decryptECB (AES.initAES key) ct

ecbDetect :: B.ByteString -> Bool
ecbDetect bs = if length blocks == length blocksNoDupes then False else True
                where blocks        = C.createBlocks bs 16
                      blocksNoDupes = nub blocks

--CBC related functions
--cbcEncrypt :: B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString
--cbcEncrypt key iv mes = cbcEncrypt' aes key iv blocks iv
--                            where blocks = C.createBlocks mes 16
--                                  aes = ecbEncrypt key 

--cbcEncrypt' aes key iv (m:ms) ct = aes (C.xor' m ct)
--cbcEncrypt' aes key iv (m:ms) ct = B.append ct' (cbcEncrypt' aes key iv ms ct')
--                                        where ct' = aes (C.xor' m ct)

cbcDecrypt :: B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString
cbcDecrypt key iv ct = cbcDecrypt' aes blocks iv
                            where blocks = C.createBlocks ct 16
                                  aes = ecbDecrypt key

--TODO: There is an off-by-one-error here. Hence, the weird pattern match for the base-case.
cbcDecrypt' :: (B.ByteString -> B.ByteString) -> [B.ByteString] -> B.ByteString -> B.ByteString
cbcDecrypt' aes (c:c1:[]) m = C.xor' (aes c) m
cbcDecrypt' aes (c:cs) m  = B.append m' (cbcDecrypt' aes cs c)
                                    where m' = C.xor' (aes c) m


--Misc. AES Functions

pkcs7 :: B.ByteString -> Int -> B.ByteString
pkcs7 bs padsize = B.append bs (B.replicate padlen pad)
                            where bslen = B.length bs
                                  padlen = padsize - (mod bslen padsize) 
                                  pad = (B.unpack $ B.singleton $ fromIntegral padlen) !! 0
