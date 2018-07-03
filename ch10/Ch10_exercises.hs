import Data.Time

stops = "pbtdkg" 
vowels = "aeiou"

combos :: [(Char, Char, Char)]
combos =  [(x, y, z) | x <- stops, y <- vowels, z <- stops]
--combos =  [(x, y, z) | x <- stops, y <- vowels, z <- stops, x == 'p']

combosThatBeginWithP :: [(Char, Char, Char)]
combosThatBeginWithP = filter isP combos
--combosThatBeginWithP = filter (\x -> fst x == 'p')

isP :: (Char, b, c) -> Bool
isP (x, _, _) = x == 'p'

combosToWords :: [(a, a, a)] -> [[a]]
combosToWords = map makeWord

makeWord :: (a, a, a) -> [a]
makeWord (x, y, z) = [x, y, z]

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime
    deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem] 
theDatabase =
    [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
    , DbNumber 9001
    , DbString "Hello, world!"
    , DbDate (UTCTime
            (fromGregorian 1921 5 1)
            (secondsToDiffTime 34123))
    ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr filterDbDate' []

filterDbDate' :: DatabaseItem -> [UTCTime] -> [UTCTime]
filterDbDate' (DbDate a) b = a : b 
filterDbDate' _ b = b

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr filterDbNumber' []

filterDbNumber' :: DatabaseItem -> [Integer] -> [Integer]
filterDbNumber' (DbNumber a) b = a : b
filterDbNumber' _ b = b

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

avgDbNumber :: [DatabaseItem] -> Double
avgDbNumber db = (fromIntegral . sumDb) db / (fromIntegral . length . filterDbNumber) db

fibs = 1 : scanl (+) 1 fibs
fibsN x = fibs !! x
fibs20 = take 20 fibs
fibsLT100 = takeWhile (< 100) fibs

factScan =  1 : scanl (*) 1 [1..]

factScan' = 1 : scanr (*) 1 [1..]

-- average length of a word in string
seekritFunc :: String -> Double
seekritFunc x = 
    fromIntegral (sum (map length (words x)))
        /  fromIntegral (length (words x))

myAnd :: [Bool] -> Bool
myAnd = foldr (&&) True

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\a b -> f a || b) False

myElem :: Eq a => a -> [a] -> Bool
myElem e = foldr (\a b -> a == e || b) False

myElem' :: Eq a => a -> [a] -> Bool
myElem' e = myAny (== e) 

myReverse :: [a] -> [a]
myReverse = foldr (\a b -> b ++ [a]) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\a b -> f a : b) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\a b -> if f a then a : b  else b ) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\a b -> f a ++ b) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
--myMaximumBy f (x:xs) = foldr (\a b -> if f a b == GT then a else b) x xs
myMaximumBy f (x:xs) = foldr (\a b ->
                                case f a b of GT -> a 
                                              _ -> b) x xs

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x:xs) = foldr (\a b ->
                                case f a b of LT -> a 
                                              _ -> b) x xs