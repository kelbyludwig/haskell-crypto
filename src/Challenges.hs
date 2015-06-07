module Challenges where

import Data.List

import qualified Encoding as E
import qualified Data.ByteString as B
import qualified Data.Bits as BIT
import qualified Crypto as C
import qualified AES as AES
import qualified Profile as P
import qualified Data.Word8 as W8

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
challenge6 :: IO String
challenge6 = do 
                f <- readFile "./src/Files/6.txt"
                let bs  = E.fromBase64 f
                return $ show $ C.breakViegnere bs

challenge7 :: IO String
challenge7 = do
                f <- readFile "./src/Files/7.txt"
                let ct = E.fromBase64 f
                let key = E.toBytes "YELLOW SUBMARINE"
                return $ show $ AES.ecbDecrypt key ct

challenge8 :: IO String
challenge8 = do
                f <- readFile "./src/Files/8.txt"
                let cts = map E.fromHex $ lines f
                let (Just index) = elemIndex True $ map AES.ecbDetect cts
                return $ show $ (index, cts !! index)

challenge9 :: IO String
challenge9 = return $ show $ AES.pkcs7 (E.toBytes "YELLOW SUBMARINE") 20

challenge10 :: IO String
challenge10 = do 
                f <- readFile "./src/Files/10.txt"
                let ct = E.fromBase64 f
                let key = E.toBytes "YELLOW SUBMARINE"
                let iv = B.replicate 16 0
                let mes = AES.cbcDecrypt key iv ct 
                let ct2 = AES.cbcEncrypt key iv mes
                return $ show (ct2 == ct)

challenge11 :: IO String
challenge11 = do
                (m,o) <- AES.createAESOracle
                return $ show (m,AES.cbcOrEbc o)

challenge12 :: IO String
challenge12 = do
                let str = E.fromBase64 "Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkg\naGFpciBjYW4gYmxvdwpUaGUgZ2lybGllcyBvbiBzdGFuZGJ5IHdhdmluZyBq\ndXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUg\nYnkK"
                oracle <- AES.createECBOracle str
                return $ show $ AES.ecbChosenPrefix oracle

challenge13 :: IO String
challenge13 = do    
                key <- AES.createAESKey
                let oracle = P.profileOracle key
                let pt1 = "AAAAAAAAAAAA" --This should put role=" at the second to last block
                let pt2 = "AAAAAAAAAAadmin\11\11\11\11\11\11\11\11\11\11\11"
                let ct1 = oracle pt1 
                let ct2 = oracle pt2
                let first  = B.take 32 ct1
                let second = B.take 16 $ B.drop 16 ct2
                return $ show $ AES.ecbDecrypt key (B.append first second)
               
challenge14 :: IO String
challenge14 = do
              let str = E.fromBase64 "Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkg\naGFpciBjYW4gYmxvdwpUaGUgZ2lybGllcyBvbiBzdGFuZGJ5IHdhdmluZyBq\ndXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUg\nYnkK"
              oracle <- AES.createECBOracleRandomPrefix str
              return $ show $ AES.ecbRandomPrefixDecrypt oracle  

challenge15 :: IO String
challenge15 = do
                let s1 = E.toBytes "ICE ICE BABY\04\04\04\04"
                let s2 = E.toBytes "ICE ICE BABY\05\05\05\05"
                let s3 = E.toBytes "ICE ICE BABY\04\03\02\01"
                let s4 = E.toBytes "ICE ICE BABY\01\02\03\04"
                putStrLn $ show $ AES.pkcs7Strip s1
                putStrLn $ show $ AES.pkcs7Strip s2
                putStrLn $ show $ AES.pkcs7Strip s3
                putStrLn $ show $ AES.pkcs7Strip s4
                return ""

challenge16 :: IO String
challenge16 = do
                key <- AES.createAESKey
                iv  <- AES.createAESKey
                let pre  = E.toBytes "comment1=cooking%20MCs;userdata="
                let post = E.toBytes ";comment2=%20like%20a%20pound%20of%20bacon"
                let filt = E.toBytes ";="
                let func = \x -> let clean = B.filter . flip B.notElem in B.concat [pre, clean filt x, post]
                let oracle = \x -> AES.cbcEncrypt key iv $ AES.pkcs7 (func x) 16
                let string = E.toBytes "AadminAtrue"
                let cipher = oracle string
                let block  = B.take 16 $ B.drop 16 $ cipher
                let bytes  = B.unpack block
                let b1     = BIT.xor W8._z (head bytes)
                let byte1  = concat [[b1], tail bytes]
                let b2     = BIT.xor W8._bar (head $ drop 6 bytes)
                let byte6  = concat [take 6 byte1, [b2], drop 7 byte1]
                let rebuild = B.pack $ concat [take 16 $ B.unpack cipher, byte6, take 16 $ drop 32 $ B.unpack cipher]
                putStrLn $ show $ AES.cbcDecrypt key iv cipher
                return $ show $ AES.cbcDecrypt key iv rebuild
