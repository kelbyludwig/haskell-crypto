module SHA1 where

import Data.ByteString as B
import Data.ByteString.Builder as BLD
import Data.ByteString.Lazy as L
import Encoding as E

--For cryptopals
challenge28 :: IO String
challenge28 = return $ show $ preprocess $ E.toBytes ""

h0 = 0x67452301
h1 = 0xEFCDAB89
h2 = 0x98BADCFE
h3 = 0x10325476
h4 = 0xC3D2E1F0

--Returns message length in bits.
mlen :: B.ByteString -> Int
mlen m = 8 * (B.length m)

len64 :: B.ByteString -> B.ByteString
len64 m = L.toStrict $ BLD.toLazyByteString $ BLD.int64BE $ fromIntegral (B.length m)

--Produces n bytes of zeros as a bytestring. 
zwords :: Int -> B.ByteString
zwords n = B.replicate n 0

--Determines the preprocessing padding size.
prepadding :: Int -> Int
prepadding = \x -> div (mod (448 - x) 512) 8

preprocess :: B.ByteString -> B.ByteString
preprocess inp = B.concat [B.snoc inp 0x80, zwords $ prepadding (ml+8), len64 inp]
                    where ml = mlen inp
                        


--TODO: Addition mod 32
modadd = undefined
