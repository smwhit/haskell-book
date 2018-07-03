import Data.Char

filterUpper :: String -> String
filterUpper = filter isUpper

capitalise :: String -> String
capitalise [] = []
capitalise (x:xs) = toUpper x : xs

allCaps :: String -> String
allCaps [] = []
allCaps (x:xs) = toUpper x : allCaps xs

allCaps' :: String -> String
allCaps' = map toUpper

firstToCaps :: String -> Char
--firstToCaps xs = (toUpper . head) xs
firstToCaps = toUpper . head

myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs) =
    if not x then
        False
    else myAnd xs

myOr :: [Bool] -> Bool
myOr [] = False
myOr (True:_) = True
myOr (_:xs) = myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny f [] = False
myAny f (x:xs) = f x || myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem y (x:xs) = y == x || myElem y xs

myElem' :: Eq a => a -> [a] -> Bool
myElem' y = myAny (==y)

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) =  x ++ squish xs 

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x:xs) = f x ++ squishMap f xs 

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:xs) = go x xs
    where
        go mx [] = mx
        go mx (y:ys) = 
            if f mx y == GT then 
                go mx ys
            else go y ys 

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x:xs) = go x xs
    where
        go mx [] = mx
        go mx (y:ys) = 
            if f mx y == LT then 
                go mx ys
            else go y ys 

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare
            
myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare
