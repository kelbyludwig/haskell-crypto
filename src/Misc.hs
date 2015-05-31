module Misc where

data Profile = Profile [(String, String)]

instance Show Profile where
    show (Profile pairs) = init $ concat $ map (\x -> concat [(fst x), "=", (show (snd x)), "&"]) pairs
    
--"Profile For" parsing routine for challenge 13
profile_for str = Profile [email, foo, role]
                  where chomper = filter . flip notElem
                        email = ("email", chomper "&=" str)
                        foo   = ("foo", "bar")
                        role  = ("role", "user")
                        
