module RegisteredUser where

newtype Username = Username String deriving Show

newtype AccountNumber = AccountNumber Integer deriving Show

data User = UnregisteredUser
        |   RegisteredUser Username AccountNumber
    deriving (Show)

printUser :: User -> IO ()
printUser UnregisteredUser = putStrLn "Unregistered User"
printUser (RegisteredUser (Username name) (AccountNumber number)) =
    putStrLn $ name ++ " " ++ show number