import Data.Monoid

a = mappend [1,2,3] [4,5,6] --[1,2,3,4,5,6]
b = mconcat [[1..3], [4..6]] --[1,2,3,4,5,6]
c = mappend "Trout" " goes well with garlic" --"Trout goes well with garlic"
d = foldr (++) [] [[1..3], [4..6]] -- [1,2,3,4,5,6]
e = foldr mappend mempty [[1..3], [4..6]]
f = getSum $ mappend (Sum 1) (Sum 1)

g = Sum 8 <> Sum 9
h = mappend mempty Sum 9
i = Sum 1 <> Sum 1 <> Sum 1
j = mconcat [Sum 8, Sum 9, Sum 10]

--left identity
li = mappend mempty Sum 1
ri = mappend (Sum 1) mempty
assoc = [1] <> ([2] <> [3])
assoc2 = [1] <> ([2] <> [3])
mc = mconcat [[1], [2], [3]]
mcf = foldr mappend mempty [[1], [2], [3]]
mconcat' = concat [[1], [2], [3]]

-- booleans
at = All True <> All True
--synonymous with
at' = mappend (All True) (All True)

nat = All True <> All False

anyt = Any True <> Any False

fj = First (Just 1) `mappend` First (Just 2)
lj = Last (Just 1) `mappend` Last (Just 2)
lj' = Last (Just 1) <> Last (Just 2)
