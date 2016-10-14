module Cipher where

import Data.Char
import Data.Bool
import System.IO

-- Only handles ['a' .. 'z'] and ['A' .. 'Z']

ceasar :: Int -> String -> String
ceasar n = toChars . encode . toInts
  where toInts  = map ord
        encode  = map (\x -> bool (lower x) (upper x) (isUpper x))
          where upper x   = 65 + ((n + x - 65) `mod` 26)
                lower x   = 97 + ((n + x - 97) `mod` 26)
                isUpper x = x < 97
        toChars = map chr

unceasar :: Int -> String -> String
unceasar n = ceasar (-n)

vigenere :: String -> String -> String
vigenere k t  = concat $ map (\x -> ceasar (shift $ snd x) [fst x]) input
  where key                 = take (length t) $ concat $ repeat k
        input               = zip t key 
        shift x | isUpper x = ord x - 65
                | otherwise = ord x - 97

main :: IO ()
main = hSetBuffering stdout NoBuffering >>
       putStr "Input text to encrypt: " >>
       getLine >>=
       (\text -> putStrLn (ceasar 5 text) >>
       putStrLn (unceasar 5 (ceasar 5 text)))
