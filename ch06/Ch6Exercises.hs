import Data.List
-- x :: Int -> Int
-- x blah = blah + 20

data Person = Person Bool deriving Show

printPerson :: Person -> IO ()
printPerson person = print person

data Mood = Blah | Woot deriving (Show, Eq)

settleDown :: Mood -> Mood
settleDown x = if x == Woot
                then Blah
                else x

type Subject = String
type Verb = String
type Object = String

data Sentence = Sentence Subject Verb Object deriving (Eq, Show)

s1 = Sentence "dogs" "drool" 
s2 = Sentence "Julie" "loves" "dogs"

data Rocks = Rocks String deriving (Eq, Show)

data Yeah = Yeah Bool deriving (Eq, Show)

data Papu = Papu Rocks Yeah deriving (Eq, Show)

i :: Num a => a
i = 1

-- f :: Fractional a => a
-- f = 1.0

f :: RealFrac a => a
f = 1.0

--freud :: a -> a
freud :: Ord a => a -> a
freud x = x

freud' :: Int -> Int
freud' x = x

myX = 1 :: Int
sigmund :: Int -> Int
--sigmund :: a -> a
sigmund x = myX

sigmund' :: Int -> Int
--sigmund' :: Num a => a -> a
sigmund' x = myX

--jung :: Ord a => [a] -> a
jung :: [Int] -> Int 
jung xs = head (sort xs)

young :: [Char] -> Char
young xs = head (sort xs)

mySort :: [Char] -> [Char]
mySort = sort

--signifier :: [Char] -> Char
signifier :: Ord a => [a] -> a
signifier xs = head (sort xs)

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f a b = f a == b 

arith :: Num b
    => (a -> b)
    -> Integer
    -> a
    -> b

arith f i a = f a + fromIntegral i