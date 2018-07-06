import Data.List
import Data.Char

data OperatingSystem = 
      GnuPlusLinux
    | OpenBSD
    | Mac
    | Windows
    deriving (Eq, Show)

data ProgLang =
      Haskell
    | Agda
    | Idris
    | PureScript
    deriving (Eq, Show)

data Programmer =
    Programmer { os:: OperatingSystem,
                 lang:: ProgLang}
    deriving (Eq, Show)

nineToFive :: Programmer
nineToFive = Programmer {
    os = Mac,
    lang = Haskell
}

feelingWizardly = 
    Programmer { 
        lang = Agda,
        os = GnuPlusLinux
    }

allOperatingSystems :: [OperatingSystem]
allOperatingSystems = [GnuPlusLinux, OpenBSD, Mac, Windows]

allLanguages :: [ProgLang]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = [ Programmer {
    lang = lang', os = os'
}| os' <- allOperatingSystems, lang' <- allLanguages ]

dd = length $ Data.List.nub allProgrammers

data Weekday =
      Monday
    | Tuesday
    | Wednesday
    | Thursday
    | Friday

f Friday = "Miller Time"

isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf _ [] = False
isSubseqOf xs ys@(y:ytail)
    | xs == take (length xs) ys = True
    | otherwise = isSubseqOf xs ytail

capitaliseWord :: String -> String
capitaliseWord (x:xs) = toUpper x : xs

capitaliseWords :: String -> [(String, String)]
capitaliseWords s = map (\x ->  (x, capitaliseWord x)) (words s)

-- myMaximumBy f (x:xs) = go x xs
--     where
--         go mx [] = mx
--         go mx (y:ys) = 
--             if f mx y == GT then 
--                 go mx ys
--             else go y ys 

splitWord c s = go (elemIndex c s) s
    where
        go (Just x) ys = let
                              current = capitaliseWord $ take (x+1) ys 
                              next = drop (x +1) ys
                              nextIndex = elemIndex c next 
                          in current : go nextIndex next
        go Nothing _ = []

capitaliseParagraph :: String -> String
capitaliseParagraph [] = []
capitaliseParagraph xs = unwords $ go (words xs) 0
    where
        go [] _ = []
        go (y:ys) n = 
            let
                z = if n < 2 then 
                        capitaliseWord y 
                        else y
                n' = if head y == '.' then 0 else n+1
            in z : go ys n' 
                    
        
