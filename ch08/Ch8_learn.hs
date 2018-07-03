factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n-1)

inc :: (Num a) => a -> a
inc n = n + 1

incTimes :: (Eq a, Num a) => a -> a -> a
incTimes x 0 = x
incTimes x n = 1 + incTimes x (n-1)

applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes 0 f b = b
--applyTimes n f b = f (applyTimes (n-1) f b)
applyTimes n f b = f . applyTimes (n-1) f $ b

f :: Bool -> Int
--f True = error "blah"
f False = 0

-- dividedBy :: Integral a => a -> a -> (a, a) 
-- dividedBy num denom = go num denom 0
--     where go n d count
--         | n < d = (count, n)
--         | otherwise = go (n - d) d (count + 1)


dividedBy :: Integral a => a -> a -> a
dividedBy num denom = fst (go num denom 0)
    where 
        go n d count
            |  n < d = (count, n)
            | otherwise = go (n - d) d (count + 1)

func :: [a] -> [a] -> [a]
func x y = x ++ y

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y 

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"