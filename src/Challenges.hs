module Challenges where

import qualified Encoding as E

challenge1 :: String
challenge1 = E.toBase64 (E.fromHex str)
                where str = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d" 
