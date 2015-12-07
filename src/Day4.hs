module Day4 where
  
import Crypto.Hash.MD5 (hash)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.ByteString.Builder (byteStringHex, toLazyByteString)

mkPrefix :: Int -> String
mkPrefix n = take n $ repeat '0'

zeroHashed :: String -> Int -> String -> Bool
zeroHashed key n = (mkPrefix n ==) . take n . LB.unpack . toLazyByteString . byteStringHex . hash . B.pack . (key ++)

answer :: String -> Int -> String
answer s n = head $ dropWhile (not . zeroHashed s n) $ map show ([1..] :: [Integer])


