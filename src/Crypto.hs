{-# LANGUAGE MultiWayIf #-}
module Crypto where

import Data.Bits
import qualified Data.List as L
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

--asciiScore :: B.ByteString -> Float
--asciiScore buf = B.foldl (\acc x -> if
--                        | W8.isAlpha x -> acc + 1.0
--                        | W8.isSpace x -> acc + 1.0
--                        | W8.isDigit x -> acc + 0.2
--                        | W8.isPunctuation x -> acc + 0.02 
--                        | otherwise -> acc - 1.0) 0.0 buf

asciiScore :: B.ByteString -> Float
asciiScore buf = B.foldl (\acc x -> if
                        | W8.isAsciiLower x -> acc + 5.0
                        | W8.isAsciiUpper x -> acc + 5.0
                        | W8.isSpace x -> acc + 2.0
                        | W8.isDigit x -> acc + 1.0
                        | W8.isPunctuation x -> acc + 1.0
                        | otherwise -> acc - 5.0) 0.0 buf


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

normalizedHammingWeight :: [B.ByteString] -> Float
normalizedHammingWeight list = (foldl (+) 0.0 scoresn) / (fromIntegral $ length scoresn)
                                  where blocksize = fromIntegral $ B.length $ list !! 0
                                        scores = zipWith hammingWeight list (drop 1 list)
                                        scoresn = map (\x -> (fromIntegral x) / blocksize) scores

breakViegnere :: B.ByteString -> B.ByteString
breakViegnere ct = B.concat (map (snd . findSingleByteXorKey . B.pack) (L.transpose blocks))
                    where keysizes  = [2..60]
                          blockList = map (createBlocks ct) keysizes
                          scores = map normalizedHammingWeight blockList
                          Just i = L.elemIndex (minimum scores) scores
                          keysize = i + 2
                          blocks = map B.unpack $ createBlocks ct keysize

--TODO: This just drops any bytes that don't cross a "size" threshold. e.g. createBlocks "YELLOW SUBMARINES" 16 would drop the last "S"
createBlocks :: B.ByteString -> Int -> [B.ByteString]
createBlocks bs size = if size <= B.length bs then x : (createBlocks xs size) else (if xs == B.empty then [] else [xs])
                       where (x,xs) = B.splitAt size bs

