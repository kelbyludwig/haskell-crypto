module Challenges where

import qualified Encoding as E
import qualified Crypto as C
import qualified Data.Word8 as W8
import qualified Data.ByteString as B

challenge1 :: String
challenge1 = E.toBase64 (E.fromHex str)
                where str = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d" 

challenge2 :: String
challenge2 = E.toHex $ C.xor' st1 st2
                where st1 = E.fromHex "1c0111001f010100061a024b53535009181c"
                      st2 = E.fromHex "686974207468652062756c6c277320657965" 
challenge3 :: String
--challenge3 = E.toHex (possibilities !! 0)
challenge3 = show $ (maximum scores, max)
                where str = E.fromHex "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
                      bytes = map B.singleton [W8._nul..]
                      possibilities = map (\key -> C.xor' key str) bytes
                      scores = map C.asciiScore possibilities
                      kv = zip scores possibilities
                      Just max = lookup (maximum scores) kv
challenge4 = "undefined"
