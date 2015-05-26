module Challenges where

import qualified Encoding as E
import qualified Crypto as C

challenge1 :: String
challenge1 = E.toBase64 (E.fromHex str)
                where str = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d" 

challenge2 :: String
challenge2 = E.toHex $ C.xor' st1 st2
                where st1 = E.fromHex "1c0111001f010100061a024b53535009181c"
                      st2 = E.fromHex "686974207468652062756c6c277320657965" 


