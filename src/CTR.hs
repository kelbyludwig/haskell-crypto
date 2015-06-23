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

--
fixedNonceAttack :: B.ByteString -> B.ByteString
fixedNonceAttack cts = C.breakViegnere cts

challenge20 :: IO String
challenge20 = do
                f <- readFile "./src/Files/20.txt"
                let ls = map E.fromBase64 $ lines f 
                let key = E.toBytes "YELLOW SUBMARINE"
                let nonce = E.toBytes "\x00\x00\x00\x00\x00\x00\x00\x00"
                let cts = map (ctrEncrypt key nonce) ls
                let l = minimum $ map B.length cts
                let newls = map (B.take l) cts
                let ctstream = B.concat newls
                let stream  = fixedNonceAttack ctstream
                return $ show $ map (\x -> B.take l $ C.xor' stream x) cts

ctrEdit :: B.ByteString -> B.ByteString -> B.ByteString -> Int -> B.ByteString -> B.ByteString
ctrEdit ct key nonce offset insert = ctrEncrypt key nonce $ B.concat [pre, insert, post]
                                     where mes  = ctrDecrypt key nonce ct
                                           len  = B.length insert
                                           pre  = B.take offset mes
                                           post = B.drop (offset + len) mes

challenge25 :: IO String
challenge25 = do 
                let ct = E.fromBase64 "L77na/nrFsKvynd6HzOoG7GHTLXsTVu9qvY/2syLXzhPweyyMTJULu/6/kXX0KSvoOLSFQ=="
                let key = E.toBytes "YELLOW SUBMARINE"
                let nonce = E.toBytes "\x00\x00\x00\x00\x00\x00\x00\x00"
                let insert = E.toBytes "test"
                let len = B.length ct
                let api = \i o -> ctrEdit ct key nonce o i --This is the API that is exposed to the attacker.
                let ctrDec = \x -> B.singleton $ B.index (api (E.toBytes "A") x) x --Insert a known letter at every index of the ct
                let as = E.toBytes $ Prelude.concat $ replicate len "A" --A bytestring of A's
                let stream = C.xor' as (B.concat $ map ctrDec [0..(B.length ct)-1]) --Recover the keystream
                return $ show $ C.xor' stream ct --Recover the plaintext from the keystream
