module Challenges where

import System.IO
import Data.List

import qualified Encoding as E
import qualified Crypto as C
import qualified Data.Word8 as W8
import qualified Data.ByteString as B


challenge1 :: IO String
challenge1 = return $ E.toBase64 (E.fromHex str)
                where str = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d" 

challenge2 :: IO String
challenge2 = return $ E.toHex $ C.xor' st1 st2
                where st1 = E.fromHex "1c0111001f010100061a024b53535009181c"
                      st2 = E.fromHex "686974207468652062756c6c277320657965" 
challenge3 :: IO String
challenge3 = return $ show $ C.xor' key str
                where str = E.fromHex "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
                      (_,key) = C.findSingleByteXorKey str 

challenge4 :: IO String
challenge4 = do
                f <- readFile "./src/Files/4.txt"
                let l = map E.fromHex $ lines f
                let possibilities =  map C.detectSingleByteXor l
                let (Just index) = elemIndex True possibilities
                let str = l !! index
                let (_,key) = C.findSingleByteXorKey str
                return $ show $ C.xor' key str

challenge5 :: IO String
challenge5 = return $ show $ E.toHex (C.xor' key str)
                where str = E.toBytes "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"
                      key = E.toBytes "ICE"
