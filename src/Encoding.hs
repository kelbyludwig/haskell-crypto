module Encoding where

import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as C8


fromHex :: String -> B.ByteString
fromHex = fst . B16.decode . C8.pack

toHex :: B.ByteString -> String
toHex = C8.unpack . B16.encode

toBase64 :: B.ByteString -> String
toBase64 = C8.unpack . B64.encode

fromBase64 :: String -> B.ByteString
fromBase64 = B64.decodeLenient . C8.pack
