lefts' :: [Either a b] -> [a]
lefts' [] = []
lefts' (x:xs) = case x of
                Left a -> a : lefts' xs
                Right _ -> lefts' xs

lefts :: [Either a b] -> [a]
lefts = foldr (\a b -> case a of 
                        Left x -> x : b
                        Right _ -> b) []

rights :: [Either a b] -> [b]
rights = foldr (\a b -> case a of 
                        Left _ -> b
                        Right x -> x : b) []

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' xs = (ls, rs)
    where
        ls = lefts xs
        rs = rights xs

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' f (Right x) = Just (f x)

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left x) = f x
either' _ f (Right x) = f x

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' _ (Left _) = Nothing
eitherMaybe'' f (Right x) = Just $ either' id f (Right x)