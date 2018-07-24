module QuickCheckExercises where

import Test.QuickCheck
import Data.List (sort)
import Data.Char (toUpper)

half x = x / 2  
halfIdentity ::  Double -> Double
halfIdentity = (*2) . half

divisor :: Gen Float
divisor = arbitrary `suchThat` (/= 0)

prop_half :: Property
prop_half =
    forAll divisor
    (\x -> half x * 2 == x)

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
    snd $ foldr go (Nothing, True) xs
    where go _ status@(_, False) = status
          go y (Nothing, t) = (Just y, t)
          go y (Just x, t) = (Just y, x >= y)

ints :: Gen [Int]
ints = arbitrary :: Gen [Int]

prop_ordered :: Property
prop_ordered =
    forAll ints
    (\x -> listOrdered (sort x))

plusAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
plusAssociative x y z =
    x + (y + z) == (x + y) + z

multAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
multAssociative x y z =
    x * (y * z) == (x * y) * z

prop_multAssociative :: Property
prop_multAssociative =
    forAll (arbitrary :: Gen Int)
    multAssociative

prop_associative :: Property
prop_associative =
    forAll (arbitrary :: Gen Int)
    plusAssociative

plusCommutative :: (Eq a, Num a) => a -> a -> Bool
plusCommutative x y =
    x + y == y + x

multCommutative :: (Eq a, Num a) => a -> a -> Bool
multCommutative x y =
    x * y == y * x

prop_multCommutative :: Property
prop_multCommutative =
    forAll (arbitrary :: Gen Int)
    multCommutative

prop_commutative :: Property
prop_commutative =
    forAll ( arbitrary :: Gen Int)
    plusCommutative

expCommutative :: Integral b => b -> b -> Bool
expCommutative a b =
    a ^ b == b ^ a

expAssociative :: (Integral a) => a -> a -> a -> Bool
expAssociative a b c =
    (a ^ b) ^ c == a ^ (b ^ c)

greaterThanZeroArbitrary :: Gen Int
greaterThanZeroArbitrary = arbitrary `suchThat` (>0)

twoNonNegative :: Gen (Int, Int)
twoNonNegative = do
    a <- greaterThanZeroArbitrary
    b <- greaterThanZeroArbitrary
    return (a, b)

threeNonNegative :: Gen (Int, Int, Int)
threeNonNegative = do
    a <- greaterThanZeroArbitrary
    b <- greaterThanZeroArbitrary
    c <- greaterThanZeroArbitrary
    return (a, b, c)

prop_expCommutative :: Property
prop_expCommutative =
    forAll twoNonNegative $ uncurry expCommutative

prop_expAssociative :: Property
prop_expAssociative =
    forAll threeNonNegative (\(x, y, z) -> expAssociative x y z)

reverseTwiceIsIdentity :: Eq a => [a] -> Bool
reverseTwiceIsIdentity xs = (reverse . reverse $ xs) == id xs

prop_reverseTwiceList :: Property
prop_reverseTwiceList = 
    forAll (arbitrary :: Gen [Int]) reverseTwiceIsIdentity

prependEqualsAppend :: Eq a => [a] -> Bool
prependEqualsAppend a = foldr (:) [] a == (++) [] a

prop_prependEqualsAppend :: Property
prop_prependEqualsAppend =
    forAll (arbitrary :: Gen [Int]) prependEqualsAppend

prop_concatEquals :: Property
prop_concatEquals =
    forAll (arbitrary :: Gen [[Int]]) (\x -> foldr (++) [] x == concat x)

funEqual :: Eq a => a -> Bool
funEqual a = id $ a == id a

funComposition :: Eq a => a -> Bool
funComposition = (\x -> id . id $ x == (id (id x)))

prop_dollarEqualApplication :: Property
prop_dollarEqualApplication =
    forAll (arbitrary :: Gen Int) funEqual

prop_funComposition :: Property
prop_funComposition =
    forAll (arbitrary :: Gen Int) funComposition

fn :: Int -> [a] -> Bool
fn n xs = length (take n xs) == n

genTuple :: Gen (Int, [Int]) 
genTuple = do
    a <- arbitrary
    b <- arbitrary
    return (a, [b])

-- property is false
prop_takeLength :: Property
prop_takeLength =
    forAll (genTuple :: Gen (Int, [Int])) (uncurry fn)

froundtrip :: (Show a, Read a, Eq a) => a -> Bool
froundtrip x = read (show x) == x

prop_roundTrip :: Property
prop_roundTrip =
    forAll (arbitrary :: Gen Int) froundtrip

square :: Num a => a -> a
square x = x * x

squareIdentity :: (Eq a, Floating a) => a -> Bool
squareIdentity x = (square . sqrt) x == x

prop_square :: Property
prop_square =
     --forAll (arbitrary :: Gen Float) squareIdentity
     collect x = (arbitrary :: Gen Float) squareIdentity

twice :: (b -> b) -> b -> b
twice f = f . f
fourTimes :: (b -> b) -> b -> b
fourTimes = twice . twice

capitaliseWord :: String -> String
capitaliseWord "" = ""
capitaliseWord (x:xs) = toUpper x : xs

f x = (capitaliseWord x == twice capitaliseWord x) 
        && (capitaliseWord x == fourTimes capitaliseWord x) 

prop_capitaliseWordIsIdempotent :: Property
prop_capitaliseWordIsIdempotent =
    forAll (arbitrary :: Gen String) f

f' :: Ord a => [a] -> Bool
f' x = sort x == twice sort x

prop_sortIsIdempotent :: Property
prop_sortIsIdempotent =
    forAll (arbitrary :: Gen [Int]) f'

data Fool =
      Fulse
    | Frue
    deriving (Eq, Show)

genFool :: Gen Fool
genFool = elements [Fulse, Frue]

genFool' :: Gen Fool
genFool' = frequency [(1, return Frue), (2, return Fulse)]

main :: IO ()
main = do
    putStrLn "half"
    quickCheck prop_half
    putStrLn "ordered"
    quickCheck prop_ordered
    print "associative"
    quickCheck prop_associative
    print "commutative"
    quickCheck prop_commutative
    print "mult associative"
    quickCheck prop_multAssociative
    print "mult commutative"
    quickCheck prop_multCommutative
    --fails
    --print "exp commutative"
    --quickCheck prop_expCommutative
    --fails
    -- print "exp associative"
    -- quickCheck prop_expAssociative
    print "twice reversed list"
    quickCheck prop_reverseTwiceList
    print "prepend equals append"
    quickCheck prop_prependEqualsAppend
    print "concat/foldr"
    quickCheck prop_concatEquals
    print "function application equals $"
    quickCheck prop_dollarEqualApplication
    print "function composition"
    quickCheck prop_funComposition
    print "square vs sqrt"
    quickCheck prop_square