module TupleFunctions where

addEmUp2 :: Num a => (a, a) -> a
addEmUp2 (x, y) = x + y

--could also be written as
addEmUp2Alt :: Num a => (a, a) -> a
addEmUp2Alt tup = fst tup + snd tup

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

third3 :: (a, b, c) -> c
third3 (_, _, x) = x

k (x, y) = x
k1 = k ((4-1), 10)
k2 = k ("three", 1+2)
k3 = k (3, True)

f :: (a, b, c)
    -> (d, e, f)
    -> ((a, d), (c, f))
f (a, b, c) (d, e, f) = ((a, d), (c, f))

pal xs =
    case xs == reverse xs of
        True -> "yes"
        False -> "no"

pal' xs = 
    case y of
        True -> "yes"
        False -> "no"
    where y = xs == reverse xs

functionC x y =
    case x > y of
        True -> x
        False -> y

ifEvenAdd2 n =
    case even n of
        True -> n+2
        _ -> n

nums x = 
    case compare x 0 of
        LT -> -1
        GT -> 1
        EQ -> 0

data Employee = Coder
        | Manager
        | Veep
        | CEO
        deriving (Eq, Ord, Show)

reportBoss :: Employee -> Employee -> IO ()
reportBoss e e' =
    putStrLn $ show e ++ " is the bosss of " ++ show e'

employeeRank :: (Employee -> Employee -> Ordering) -> Employee -> Employee -> IO ()
employeeRank f e e' =
    case f e e' of
        GT -> reportBoss e e'
        EQ -> putStrLn "Neither employee is the boss"
        LT -> (flip reportBoss) e e'

codersRuleCEOSDrool :: Employee -> Employee -> Ordering
codersRuleCEOSDrool Coder Coder = EQ
codersRuleCEOSDrool Coder _ = GT
codersRuleCEOSDrool _ Coder = LT
codersRuleCEOSDrool e e' = compare e e'

dodgy :: Num a => a -> a -> a
dodgy x y = x + y * 10

oneIsOne :: Num a => a -> a
oneIsOne = dodgy 1

oneIsTwo :: Num a => a -> a
oneIsTwo = flip dodgy 2