data Mood = Blah | Woot deriving Show

changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood _ = Blah

greetIfCool :: String -> IO ()
greetIfCool greeting =
    if cool greeting then
        putStrLn "That's cool"
    else 
        putStrLn "Oh!"
    where cool x =
            x == "Hello"

tupFunc :: (Int, [a]) -> (Int, [a]) -> (Int, [a])
tupFunc (a,b) (c, d) = (a+c, b ++ d)

isPalindrome :: Eq a =>  [a] -> Bool
isPalindrome x = reverse x == x

myAbs :: Integer -> Integer
myAbs x = if x < 0 then
            -x
          else x

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f a b = ((snd a, snd b), (fst a, fst b))

addStuff :: Integer -> Integer -> Integer
addStuff a b = a + b + 5

addThree a b c = a + b + c

subtractStuff x y = x - y - 10

g :: a -> b -> c -> b
g x y z = y

h :: (Num a, Num b) => a -> b -> b
h a b = b

jackal :: (Ord a, Eq b) => a -> b -> a
jackal a b = a

kessel :: (Ord a, Num b) => a -> b -> a
kessel a b = a

myId :: a -> a 
myId a = a

twoAs :: a -> a -> a
twoAs a b = b

aAndB :: a -> b -> b
aAndB a b = b

aAndB2 :: (Num a, Num b) -> a -> b -> b
aAndB2 a b = if a == b then a else b