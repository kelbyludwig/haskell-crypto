module Profile where

import qualified Data.List.Split as S
import qualified Data.ByteString as B
import qualified AES as AES
import qualified Encoding as E

data Profile = Profile [(String, String)]

instance Show Profile where
    show (Profile pairs) = init $ concat $ map (\x -> concat [(fst x), "=", (snd x),"&"]) pairs
    
--"Profile For" parsing routine for challenge 13
profileFor :: String -> Profile 
profileFor str = Profile [email, foo, role]
                  where chomper = filter . flip notElem
                        email = ("email", show $ chomper "&=" str)
                        foo   = ("foo", show "bar")
                        role  = ("role", show "user")

profileParse :: String -> [(String, String)]
profileParse str = pairs
                    where kvs     = S.splitOn "&" str
                          cleaner = \x -> let (k:v:[]) = S.splitOn "=" x in (k,v)
                          pairs   = map cleaner kvs

--Haskell "show" is a pain. It adds too many slashes. The hardcoded "ergh" saved me time :P
profileOracle :: B.ByteString -> String -> B.ByteString
profileOracle key str = AES.ecbEncrypt key $ AES.pkcs7 (E.toBytes ergh) 16
                        where ergh = "email=" ++ str ++ "&foo=bar&role=user"
