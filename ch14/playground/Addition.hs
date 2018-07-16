module Addition where

import Test.Hspec
import Test.QuickCheck

sayHello :: IO ()
sayHello = putStrLn "hello!"

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
    where 
        go n d count
          | n < d = (count,n)
          | otherwise = go (n - d) d (count + 1)

addOneIfOdd :: (Integral a) => a -> a
addOneIfOdd n =
    if odd n then
        n + 1
    else n
    
main :: IO ()
main = hspec $
    describe "Addition" $ do
        it "1 + 1 is greater than 1" $
            (1 + 1) > 1 `shouldBe` True
        it "2 + 2 is equal to 4" $
            2 + 2 `shouldBe` 4
        it "15 divided by 3 is 5" $
            15 `dividedBy` 3 `shouldBe` (5, 0)
        it "22 divided by 5 is\
            \ 4 reminder 2" $
            (22 :: Int) `dividedBy` 5 `shouldBe` (4, 2)
        it "odd adds one" $
            addOneIfOdd  (1::Int) `shouldBe` (2 :: Int) 
        it "even returns number" $
            addOneIfOdd (2 :: Int) `shouldBe` (2 :: Int)
        it "x + 1 is always\
            \ greater than x" $
            property $ \x -> x + 1 > (x :: Int)

trivialInt :: Gen Int
trivialInt = return 1

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True, True, True, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a'.. 'z']

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
    a <- arbitrary
    b <- arbitrary
    return (a, b)

-- sample (genTuple :: Gen ([()], Char))

genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
genEither = do
    a <- arbitrary
    b <- arbitrary
    elements [Left a, Right b]

-- sample' (genEither :: Gen (Either Int Int))

genMaybe :: Arbitrary a => Gen (Maybe a)
genMaybe = do
    a <- arbitrary
    elements [Nothing, Just a]

genMaybe' :: Arbitrary a => Gen (Maybe a)
genMaybe' = do
    a <- arbitrary
    frequency [(1, return Nothing),
                (3, return (Just a))]
--sample' (genMaybe' :: Gen (Maybe Int))
--sample' (genMaybe' :: Gen (Maybe [Int]))

prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x

runQc :: IO ()
runQc = quickCheck prop_additionGreater