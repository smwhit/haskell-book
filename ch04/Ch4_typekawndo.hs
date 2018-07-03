module TypeKwanDo where

import Data.Char

f :: Int -> String
f x = show x

g :: String -> Char
g = head

h :: Int -> Char
h x = chr x

data A
data B
data C

q :: A -> B
q = undefined

w :: B -> C
w = undefined

e :: A -> C
--e a = w (q a)
e a = w $ q a

data X
data Y
data Z

xz :: X -> Z
xz = undefined

yz :: Y -> Z
yz = undefined

xform :: (X, Y) -> (Z, Z)
xform (x, y) = (xz x , yz y)

munge :: (x -> y)
         -> (y -> (w, z))
         -> x
         -> w
munge fx fy x = fst (fy (fx x))