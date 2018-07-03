-- module ModuleWithWhere where

-- printInc2 n = print plusTwo
--     where plusTwo = n + 2
 
-- module ModuleWithLet where
    
-- printInc2 n = let printTwo = n + 2
--              in print printTwo
f =
    let x = 5
        y = 6
    in x * y

--let x = 3; y = 1000 in x * 3 + y
f2 = x * 3 + y
    where x = 3
          y = 1000

-- let y = 10; x = 10 * 5 + y in x * 5
f3 = x * 5
    where y = 10
          x = 10 * 5 + y

f4 =          
    let x = 7
        y = negate x
        z = y * 10 
    in z / x + y

f5 = z / x + y
    where 
        x = 7
        y = negate x
        z = y * 10

waxOn = x * 5
    where
        z = 7
        y = z + 8
        x = y ^ 2

waxOn2 = let
            z = 7
            y = z + 8
            x = y ^ 2
         in x * 5

triple x = x * 3

waxOff x = let
            y = x * 2
            z = triple y
           in 
            z

area d = pi * (r * r)
    where r = d / 2