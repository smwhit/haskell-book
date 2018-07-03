sumTo :: (Eq a, Num a) => a -> a
sumTo num = go num 0 1
    where
        go n total from
            | n == from-1 = total
            | otherwise = go n (total + from) (from+1)

mult :: (Integral a) => a -> a -> a
mult x y = go x y 0
        where 
            go x y total
                | y < 1 = total
                | otherwise = go x (y-1) (total + x)

dividedBy :: Integral a => a -> a -> a
dividedBy num denom = fst (go num denom 0)
    where 
        go n d count
            | n < d = (count, n)
            | otherwise = go (n - d) d (count + 1)

data DividedResult =
        Result Integer
    |   DividedByZero
    deriving Show
    
--fixedDividedBy :: Integral a => a -> a -> DividedResult
fixedDividedBy num denom = go num denom 0
    where
        go n d count
            | d == 0 = DividedByZero
            | n < abs d = Result count
            -- | otherwise = go (n - d) d (count + signum d )
            | otherwise = go (n - d) d (count + signum d)

mc91 :: Integer -> Integer
mc91 n 
    | n > 100 = n - 10
    | otherwise = (mc91 . mc91) (n + 11)

mc91' :: Integer -> Integer
mc91' n = case n > 100 of
    True -> n - 10
    False -> mc91' (mc91' (n+11))
    -- | otherwise = mc91' mc91 (n+11)

mc91'' :: Integer -> Integer
mc91'' n = if n > 100 then
            n - 10
           else
            (mc91'' . mc91'') (n+11)
    

