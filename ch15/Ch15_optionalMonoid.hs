import Data.Monoid

data Optional a =
      Nada
    | Only a
    deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
    mempty = Nada
    mappend Nada Nada = Nada
    mappend Nada (Only a) = Only a
    mappend (Only a) Nada = Only a
    mappend (Only a) (Only b) = Only (a `mappend` b)

a = Only (Sum 1) `mappend` Only (Sum 1)
b = Only (Product 4) `mappend` Only (Product 2)
c = Only (Sum 1) `mappend` Nada
d = Only [1] `mappend` Nada
e = Only [1] `mappend` Only [2]