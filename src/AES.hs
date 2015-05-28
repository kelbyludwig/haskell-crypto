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
