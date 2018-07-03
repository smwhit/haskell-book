import Data.Bool

myHead :: [a] -> a
myHead (x:_) = x

safeMyHead :: [a] -> Maybe a
safeMyHead [] = Nothing
safeMyHead (x:_) = Just x

eftBool :: Bool -> Bool -> [Bool]
eftBool False True = [False, True]
eftBool True False = [True, False]
eftBool b _ = [b]

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd x y
    | x == GT && y == LT || y == EQ = []
    | x == GT = [GT]
    | otherwise = x :  eftOrd (succ x) (succ y)

eftInt :: Int -> Int -> [Int]
eftInt x y 
    | x > y = []
    | otherwise = x : eftInt (succ x) y

eftChar :: Char -> Char -> String
eftChar x y 
    | x > y = []
    | otherwise = x : eftChar (succ x) y

eftGeneric :: (Enum a, Ord a) => a -> a -> [a]
eftGeneric x y 
    | x > y = []
    | otherwise = x : eftGeneric (succ x) y

myWords :: String -> [String]
myWords [] = []
myWords s = takeWhile (/=' ') s : myWords (dropWhile (==' ') (dropWhile (/=' ') s))

acro :: String -> String
acro xs = [x | x <- xs, x `elem` ['A'..'Z']]

mySqr = [x^2 | x <- [1..5]]
myCube = [y^3 | y <- [1..5]]

tuples = [(x, y) | x<- mySqr, y <- myCube, x < 50, y < 50]

-- spine exercises
--1
a = [x^y | x <- [1..5], y <- [2, undefined]]
b = take 1 a
c = sum [1, undefined, 3]
d = length [1, 2, undefined]
e = length $ [1,2,3] ++ undefined
f = take 1 $ filter even [1,2,3,undefined]
g = take 1 $ filter even [1,3, undefined]
h = take 1 $ filter odd [1,3,undefined]
i = take 2 $ filter odd [1,3,undefined]
j = take 3 $ filter odd [1,3,undefined]

-- transforming/mapping
itIsMystery :: String -> [Bool]
itIsMystery = map (`elem` "aeiou")

aa = map (^2) [1..10]
bb = map minimum [[1..10], [10..20], [20..30]]
cc = map sum [[1..5], [1..5], [1..5]]

--map (\x -> if x == 3 then (-x) else (x)) [1..10]
dd = map (\x -> bool x (negate x) (x==3)) [1..10]

--filtering

ee = filter (\x -> x `mod` 3 == 0) [1..30]

myFilter xs = filter (\x -> x /= "the" && x /= "a") (words xs)
myFilter' xs = filter (\x -> x `notElem` ["the", "an", "a"]) (words xs)

-- zipping
zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys 

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = [] 
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys 

zip'' :: [a] -> [b] -> [(a, b)]
zip'' = zipWith (\x y -> (x, y))