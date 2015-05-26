{-# LANGUAGE MultiWayIf #-}
module Crypto where

import Data.Bits
import qualified Data.ByteString as B
import qualified Data.Word8 as W8

xor' :: B.ByteString -> B.ByteString -> B.ByteString
xor' key buf = case B.length key `compare` B.length buf of
                    LT -> if (remlen == 0) then header else B.append header footer
                        where keylen = B.length key
                              buflen = B.length buf
                              remain = keylen * (quot buflen keylen)
                              remkey = B.concat (Prelude.replicate remain key)
                              header = xor' remkey (B.take remain buf)
                              remlen = B.length (B.drop remain buf)
                              footer = xor' (B.take remlen key) (B.drop remain buf)
                    GT -> B.pack (B.zipWith xor k buf)
                        where l = B.length buf
                              k = B.take l key
                    EQ -> B.pack (B.zipWith xor key buf)

--TODO: Use list comprehension to create a list of 256 different byte values
--TODO: Data.CHar is useful
--TODO: ['\0'..'\255'] viola!
--  Add singleton to make it a Char8

asciiScore :: B.ByteString -> Float
asciiScore bs = B.foldl (\acc x -> if
                        | W8.isAlpha x -> acc + 1.0
                        | W8.isSpace x -> acc + 1.0
                        | W8.isDigit x -> acc + 0.2
                        | W8.isPunctuation x -> acc + 0.4
                        | otherwise -> acc + 0.0) 0.0 bs
