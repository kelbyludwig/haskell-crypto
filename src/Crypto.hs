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

asciiScore :: B.ByteString -> Float
asciiScore buf = B.foldl (\acc x -> if
                        | W8.isAlpha x -> acc + 1.0
                        | W8.isSpace x -> acc + 1.0
                        | W8.isDigit x -> acc + 0.2
                        | W8.isPunctuation x -> acc + 0.4
                        | otherwise -> acc + 0.0) 0.0 buf

findSingleByteXorKey :: B.ByteString -> (Float, B.ByteString)
findSingleByteXorKey buf = (maximum scores, key) 
                             where bytes = map B.singleton [W8._nul..]
                                   possibilities = map (\k -> xor' k buf) bytes
                                   scores = map asciiScore possibilities
                                   kv = zip scores bytes
                                   Just key = lookup (maximum scores) kv

--The comparison value is adjustable.
detectSingleByteXor :: B.ByteString -> Bool
detectSingleByteXor buf = if average > 0.92 then True else False
                            where (score, _) = findSingleByteXorKey buf
                                  average = score / fromIntegral (B.length buf)

hammingWeight :: B.ByteString -> B.ByteString -> Int
hammingWeight bs1 bs2 = B.foldl hm 0 bsx 
                            where bsx = xor' bs1 bs2
                                  hm = \acc x -> acc + popCount x

