{-# LANGUAGE NoMonomorphismRestriction #-}
module DetermineTheType where

example = (* 9 ) 6
example2 = head [(0, "doge"), (1, "kitteh")]
example3 = head [(0 :: Integer, "doge"), (1, "kitteh")]
example4 = if False then True else False
example5 = let
            x = 5
            y = x + 5
            w = y * 10
           in w

example6 = let
            x = 5
            y = x + 5
            z y = y * 10
           in z

example7 = let 
            x = 5
            y = x + 5
            f = 4 / y
           in f

example8 = let
            x = "Julie"
            y = " <3"
            z = "Haskell"
            f = x ++ y ++ z
           in f

--let x = (* 9) 6

functionH :: [a] -> a
functionH (x:_) = x

functionC :: Ord a => a -> a -> Bool
functionC x y =
    --if (x > y) then True else False
    x > y

functionS :: (a, b) -> b
functionS (_, y) = y

i :: a -> a
i a = a

c :: a -> b -> a
c a _ = a

c'' :: b -> a -> b
c'' a _ = a

c' :: a -> b -> b
c' _ b = b

r :: [a] -> [a]
--r a = reverse a
--r a = a
r a = take 2 a

co :: (b -> c) -> (a -> b) -> a -> c
co f g a = f(g(a))

a :: (a -> c) -> a -> a
a f a = a

a' :: (a -> b) -> a -> b
a' f = f
