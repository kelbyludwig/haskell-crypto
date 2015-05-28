module AES where

import qualified Crypto.Cipher.AES as AES
import qualified Data.ByteString as B

ecbEncrypt :: B.ByteString -> B.ByteString -> B.ByteString
ecbEncrypt key mes = AES.encryptECB (AES.initAES key) mes

ecbDecrypt :: B.ByteString -> B.ByteString -> B.ByteString
ecbDecrypt key ct = AES.decryptECB (AES.initAES key) ct
