module CTR where

import qualified Crypto as C
import qualified Encoding as E
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified AES as AES
import qualified Data.Binary.Builder as BU

ctrEncrypt :: B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString 
ctrEncrypt key nonce mes = B.take (B.length mes) $ C.xor' mes stream
                            where blocks  = C.createBlocks mes 16
                                  numcntr = (length blocks) + 2
                                  stream  = B.concat $ ctrStream key nonce 0 numcntr 
                                  
ctrStream :: (Integral a) => B.ByteString -> B.ByteString -> a -> Int -> [B.ByteString]
ctrStream key nonce ctr 0      = [AES.ecbEncrypt key (B.append nonce n)]
                                    where n = ctrBytes ctr
ctrStream key nonce ctr blocks = AES.ecbEncrypt key (B.append nonce n) : ctrStream key nonce (ctr+1) (blocks-1)
                                    where n = ctrBytes ctr

ctrDecrypt :: B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString
ctrDecrypt key nonce cip = ctrEncrypt key nonce cip


ctrBytes :: (Integral a) => a -> B.ByteString
ctrBytes num = B.concat $ LB.toChunks $ BU.toLazyByteString $ BU.putWord64le (fromIntegral num)

challenge18 :: IO String
challenge18 = do
               let str = E.fromBase64 "L77na/nrFsKvynd6HzOoG7GHTLXsTVu9qvY/2syLXzhPweyyMTJULu/6/kXX0KSvoOLSFQ=="
               let nonce = E.toBytes "\x00\x00\x00\x00\x00\x00\x00\x00" 
               let key = E.toBytes "YELLOW SUBMARINE"
               return $ show $ ctrDecrypt key nonce str
