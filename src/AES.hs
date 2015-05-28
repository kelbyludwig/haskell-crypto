module AES where

import Data.List
import qualified Crypto.Cipher.AES as AES
import qualified Data.ByteString as B
import qualified Crypto as C
import qualified Data.Word8 as W8

--ECB related functions
ecbEncrypt :: B.ByteString -> B.ByteString -> B.ByteString
ecbEncrypt key mes = AES.encryptECB (AES.initAES key) mes

ecbDecrypt :: B.ByteString -> B.ByteString -> B.ByteString
ecbDecrypt key ct = AES.decryptECB (AES.initAES key) ct

ecbDetect :: B.ByteString -> Bool
ecbDetect bs = if length blocks == length blocksNoDupes then False else True
                where blocks        = C.createBlocks bs 16
                      blocksNoDupes = nub blocks


--Misc. AES Functions

pkcs7 :: B.ByteString -> Int -> B.ByteString
pkcs7 bs padsize = B.append bs (B.replicate padlen pad)
                            where bslen = B.length bs
                                  padlen = padsize - (mod bslen padsize) 
                                  pad = (B.unpack $ B.singleton $ fromIntegral padlen) !! 0
