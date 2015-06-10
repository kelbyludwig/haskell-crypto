module CBCPaddingOracle where

import qualified Encoding as E
import qualified System.Random as R
import qualified AES as AES
import qualified Data.ByteString as B

challenge17 :: IO String
challenge17 = do
                f <- readFile "./src/Files/17.txt"
                let ls = map E.fromBase64 $ lines f
                g <- R.newStdGen 
                let i = head $ take 1 (R.randomRs (0,9) g)
                let line = ls !! i
                key <- AES.createAESKey
                iv  <- AES.createAESKey
                let oracle = \x -> (iv, AES.cbcEncrypt key iv $ AES.pkcs7 x 16)               
                let (_, ct) = oracle line
                return $ show $ paddingOracle key iv ct

paddingOracle :: B.ByteString -> B.ByteString -> B.ByteString -> Bool
paddingOracle key iv bs = case AES.pkcs7Strip pt of
                            Left  _ -> False
                            Right _ -> True
                          where pt = AES.cbcDecrypt key iv bs
