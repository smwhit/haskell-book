module Ch3Exercises where

dropX x s = 
    drop x s

concatWithEx s =
    s ++ "!"

getFourthCharAsString x = 
    take 1 $ drop 4 x

thirdLetter :: String -> Char
thirdLetter x = x !! 2

letterIndex :: Int -> Char
letterIndex n = "Curry is awesome" !! n

rvrs :: String -> String
rvrs s =
    drop 9 s ++ " " ++  take 3 (drop 6 s) ++ take 5 s