notThe :: String -> Maybe String
notThe s
    | s == "the" = Nothing
    | otherwise = Just s

replaceThe :: String -> String
replaceThe s = unwords $ go (words s)
    where
        go = fmap (\x -> case notThe x of
            Nothing -> "a"
            _ -> x)

countTheBeforeVowell :: String -> Integer
countTheBeforeVowell s = go $ zip (words s) (tail $ words s)
    where
        go = foldr (\a b -> if isTheOrVowell a then b+1 else b) 0
        isTheOrVowell t = fst t == "the" && head (snd t) `elem` ['a', 'e', 'i', 'o', 'u']

countTheBeforeVowell' :: String -> Integer
countTheBeforeVowell' s = let
                            ws = words s
                            tws = tail ws
                            res = go $ zip ws tws
                          in res
    where
        go = foldr (\a b -> if isTheOrVowell a then b+1 else b) 0
        isTheOrVowell t = isThe t && startsWithVowell t
        isThe t = fst t == "the"
        startsWithVowell t = head (snd t) `elem` ['a', 'e', 'i', 'o', 'u']

isVowell c = c `elem` ['a', 'e', 'i', 'o', 'u']

countVowels s = foldr (\a b -> if isVowell a then b+1 else b) 0 s

newtype Word' = Word' String deriving (Eq, Show)

vowels = "aeiou"
mkWord :: String -> Maybe Word'
mkWord xs =
    let
        cv = foldr (\a b -> if a `elem` vowels then (fst b+1, snd b) else (fst b, snd b+1)) 
                (0,0) xs
        w = if uncurry (>) cv then Nothing else Just (Word' xs)
    in w