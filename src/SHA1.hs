module SHA1 where

import Data.ByteString as B
import Data.ByteString.Builder as BLD
import Data.ByteString.Lazy as L
import Encoding as E
import Data.List.Split

--For cryptopals
challenge28 :: IO String
challenge28 = return $ show (chunk512 $ preprocess $ B.replicate 56 0)

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
                   
chunk512 :: B.ByteString -> [B.ByteString]     
chunk512 inp = Prelude.map B.pack (chunk 64 $ B.unpack inp)

--TODO: This may be useful: https://mail.haskell.org/pipermail/beginners/2010-October/005571.html
chunk32 :: B.ByteString -> [Word32]
chunk32 inp = B.unpack inp
