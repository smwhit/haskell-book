mTh x y z = x * y * z
mTh' x y = \z -> x * y * z
mTh'' x = \y -> \z -> x * y * z
mTh''' = \x -> \y -> \z -> x * y * z

addOneIfOdd = \n -> case odd n of
    True -> f n
    False -> n
    where f n = n + 1

addFive = \x -> \y -> (if x > y then y else x) + 5

mflip f x y = f y x

data WherePenguinsLive =
        Galapagos
    |   Antartica
    |   Australia
    |   SouthAfrica
    |   SouthAmerica
    deriving (Eq, Show)

data Penguin = 
    Peng WherePenguinsLive
    deriving (Eq, Show)

isSouthAfrica :: WherePenguinsLive -> Bool
isSouthAfrica SouthAfrica = True
isSouthAfrica _           = False

gimmeWhereTheyLive :: Penguin -> WherePenguinsLive
gimmeWhereTheyLive (Peng whereitlives) = whereitlives

humboldt = Peng SouthAmerica
gentoo = Peng Antartica
macaroni = Peng Antartica
little = Peng Australia
galapagos = Peng Galapagos

galapagosPenguin :: Penguin -> Bool
galapagosPenguin (Peng Galapagos) = True
galapagosPenguin _ = False

antarcticPenguin :: Penguin -> Bool
antarcticPenguin (Peng Antartica) = True
antarcticPenguin _ = False

antarcticOrGalapagos :: Penguin -> Bool
antarcticOrGalapagos p = galapagosPenguin p || antarcticPenguin p

myAbs :: Integer -> Integer
myAbs x
    | x < 0 = -x
    | otherwise = x

bloodNa :: Integer -> String
bloodNa x
    | x < 135 = "too low"
    | x > 145 = "too high"
    | otherwise = "just right"

isRight :: (Num a, Eq a) => a -> a -> a -> String
isRight a b c
    | a^2 + b^2 == c^2 = "Right on"
    | otherwise = "no"

avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
    -- | otherwise = 'F'
    | y >= 0.9 = 'A'
    | y >= 0.8 = 'B'
    | y >= 0.7 = 'C'
    | y >= 0.59 = 'D'
    | otherwise = 'F'
    where y = x / 100

numbers :: (Ord a, Num a) => a -> a
numbers x
    | x < 0 = -1
    | x == 0 = 0
    | x > 0 = 1

f x = take 5 . filter odd . enumFrom $ x
--f x = take 5 . filter odd . enumFrom $ x