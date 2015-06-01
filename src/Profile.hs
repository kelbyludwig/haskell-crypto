module Profile where

import qualified Data.List.Split as S
import qualified Data.ByteString as B
import qualified AES as AES
import qualified Encoding as E

data Profile = Profile [(String, String)]

instance Show Profile where
    show (Profile pairs) = init $ concat $ map (\x -> concat [(fst x), "=", (show (snd x)), "&"]) pairs
    
--"Profile For" parsing routine for challenge 13
profileFor str = Profile [email, foo, role]
                  where chomper = filter . flip notElem
                        email = ("email", chomper "&=" str)
                        foo   = ("foo", "bar")
                        role  = ("role", "user")

profileParse str = pairs
                    where kvs     = S.splitOn "&" str
                          chomper = filter . flip notElem
                          cleaner = \x -> let (k:v:[]) = S.splitOn "=" x in (chomper "\"\\" k, chomper "\"\\" v)
                          pairs   = map cleaner kvs

profileOracle :: B.ByteString -> String -> B.ByteString
profileOracle key str = AES.ecbEncrypt key $ AES.pkcs7 (E.toBytes (show $ profileFor str)) 16

