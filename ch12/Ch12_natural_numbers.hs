data Nat =
    Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = 1 + natToInteger n

integerToNat :: Integer -> Maybe Nat
integerToNat n
   | n < 0 = Nothing
   | otherwise = Just (getNat n)
   where getNat 0 = Zero
         getNat n' = Succ (getNat (n'-1))