f :: Char -> String
f = undefined

g :: String -> [String]
g = undefined

f2 :: Ord a => a -> a -> Bool
f2 = undefined 

f3 :: a -> a
f3 x = x

tensDigit :: Integral a => a -> a
tensDigit x = d
    where xLast = x `div` 10
          d = xLast `mod` 10

tensDigit' :: Integral a => a -> a
tensDigit' x = snd (x `divMod` 10)

tensDigit'' :: Integral a => a -> a
tensDigit'' x =
    let 
        y = x `divMod` 10
    in snd y

tensDigit''' :: Integral a => a -> a
tensDigit''' x = d
    where y = x `divMod` 10
          d = snd y

hunsD x = d2
    where y = x `divMod` 100
          d2 = snd y

foldBool :: a -> a -> Bool -> a
foldBool x y b =
    case b == False of
        True -> x
        False -> y

foldBool' :: a -> a -> Bool -> a
foldBool' x y b
 | b == False = x
 | b == True  = y

foldBool3 :: a -> a -> Bool -> a
foldBool3 x _ False = x
foldBool3 _ y True = y

g2 :: (a -> b) -> (a, c) -> (b, c)
g2 f' (a, c) = (f' a, c) 
