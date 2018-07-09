isJust :: Maybe a -> Bool
isJust Nothing = False
isJust (Just _) = True

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

maybee :: b -> (a -> b) -> Maybe a -> b
maybee x _ Nothing = x
maybee _ f (Just a) = f a

fromMaybe :: a -> Maybe a -> a
fromMaybe a Nothing = a
fromMaybe _ (Just a) = a

fromMaybe' :: a -> Maybe a -> a
fromMaybe' a = maybee a id

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x

catMaybes :: [Maybe a] -> [a]
catMaybes m
    | [] <- m = []
    | (x:xs) <- m = case x of
          Nothing -> catMaybes xs
          Just a -> a : catMaybes xs

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe [] = Just []
flipMaybe ms = let
                 xs = Just (catMaybes ms)
                 c = if length xs == length ms then xs else Nothing
               in c