myLength xs = foldr (\a b -> b + 1) 0 xs
mySum xs = foldr (\a b -> a + b) 0 xs
myProduct xs = foldr (\a b -> a * b) 1 xs
myConcat xs = foldr (\a b -> a ++ b) [] xs

xs = map show [1..5]
y = foldr (\x y -> concat ["(",x,"+",y,")"]) "0" xs
z = foldl (\x y -> concat ["(",x,"+",y,")"]) "0" xs
aa = foldl (\x y -> concat ["(",x,"-",y,")"]) "0" xs
bb = foldl (\x y -> x - y) 0 [1..5]
cc = foldr (\x y -> concat ["(",x,"-",y,")"]) "0" xs


add :: Num a => a -> a -> a -> a
add a b c = a + b * c

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ z [] = z
myFoldr f z (x:xs) = f x (myFoldr f z xs)

myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl _ acc [] = acc
myFoldl f acc (x:xs) = myFoldl f (f acc x) xs 

dd = foldr (++) "" ["woot", "woot", "WOOT"]
ee = foldr max [] ["fear is the little death"]
ff = foldr (&&) True [False, True]
gg = foldr ((++) . show) "" [1..5]
hh = foldr const 'a' ['b'..'f']
ii = foldl const 0 ["tacos"]
jj = foldr (flip const) 0 ["burritos"]
kk = foldr (flip const) 'z' [1..5]

pab =  let 
        p = ["Pizza", "Apple", "Banana"]
     in p
     
t3 = foldr (\a b -> take 3 (a:: String) ++ (b :: String)) [] pab

