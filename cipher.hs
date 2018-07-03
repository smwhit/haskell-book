module Cipher where

import Data.Char

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Char -> Int -> Char
shift c s = int2let (((let2int c) + s) `mod` 26)

caesar :: Int -> String -> String
caesar s xs = map (\x -> shift x s) xs

uncaesar :: Int -> String -> String
uncaesar n xs = caesar (-n) xs