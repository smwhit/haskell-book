import Control.Monad
import System.Exit (exitSuccess)
import Data.Char

palindrome :: IO ()
palindrome = forever $ do
    line1 <- getLine
    case (palindrome' line1) of
        True -> putStrLn "It's a palindrome!"
        False -> do
             putStrLn "Nope!"
             exitSuccess

palindrome' :: String -> Bool
palindrome' s = 
    let
        n = normalise s
    in n == reverse n
    where 
        normalise [] = []
        normalise (x:xs)
            | isAlpha x = toLower x : normalise xs
            | otherwise = normalise xs
            -- case isAlpha x of
            --     True -> toLower x : normalise xs
            --     False -> normalise xs

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid =
      NameEmpty
    | AgeTooLow
    | PersonInvalidUnknown String
    deriving (Eq, Show)

mkPerson ::  String -> Integer -> Either PersonInvalid Person
mkPerson name age
    | name /= "" && age > 0 =
        Right $ Person name age
    | name == "" = Left NameEmpty
    | not (age > 0) = Left AgeTooLow
    | otherwise = Left $ PersonInvalidUnknown $
        "Name was: " ++ show name ++
        " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
    putStr "Enter a name: "
    name <- getLine
    putStr "Enter an age: "
    age <- getLine
    case mkPerson name (read age) of
        Right (Person _ _) -> putStrLn "Yay"
        Left invalid -> putStrLn ("oh dear " ++ show invalid)
    return ()