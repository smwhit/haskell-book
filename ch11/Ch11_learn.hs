{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
import Data.Int

data PugType = PugData

data HuskyType a = HuskyData

data DogueDeBordeaux doge = DogueDeBordeaux doge

myPug = PugData :: PugType

myHusky :: HuskyType a
myHusky = HuskyData

myOtherHusky :: Num a => HuskyType a
myOtherHusky = HuskyData

myOtherOtherHusky :: HuskyType [[[[Int]]]]
myOtherOtherHusky = HuskyData

myDoge :: DogueDeBordeaux Int
myDoge = DogueDeBordeaux 10

-- badDoge :: DogueDeBordeaux String
-- badDoge = DogueDeBordeaux 10

data Doggies a = 
        Husky a
    |   Mastiff a
    deriving (Eq, Show)

-- Vehicle example

data Price = Price Integer deriving (Eq, Show)

data Manufacturer = Mini
                    | Mazda
                    | Tata
                    deriving (Eq, Show)

data Airline = PapuAir
               | CatapultsR'Us
               | TakeYouChancesUnited
               deriving (Eq, Show)

data Size = Size Integer deriving (Eq, Show)

data Vehicle = Car Manufacturer Price
             | Plane Airline Size
             deriving (Eq, Show)

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir (Size 1000)

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane  _ _) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

getManu :: Vehicle -> Manufacturer
getManu (Car m _) = m

data Example = MakeExample deriving Show

data Example2 = MakeExample2 Int deriving Show

class TooMany a where
    tooMany :: a -> Bool

instance TooMany Int where
    tooMany n = n > 42

newtype Goats = Goats Int deriving (Eq, Show, TooMany)
-- instance TooMany Goats where
--     tooMany (Goats n) = n > 43

--newtype TooManyTuple = TooManyTuple Int deriving (Eq, Show, TooMany)
instance TooMany (Int, String) 
    where tooMany (i, _) = i > 42

instance TooMany (Int, Int)
    where tooMany (i, j) = i + j > 42

instance (Num a, TooMany a) => TooMany (a, a)
    where tooMany (b, c) = tooMany (b + c)

data BigSmall =
      Big Bool
    | Small Bool
    deriving (Eq, Show)

data NumberOrBool =
      Numba Int8
   |  BoolyBool Bool
   deriving (Eq, Show)

data Person =
    Person { 
        name :: String,
        age :: Int
    }
    deriving (Eq, Show)

data Person2 = MkPerson String Int deriving (Eq, Show)

personAge :: Person2 -> Int
personAge (MkPerson _ a) = a

personName :: Person2 -> String
personName (MkPerson n _) = n

data Fiction = Fiction deriving Show
data Nonfiction = NonFiction deriving Show

data BookType = FictionBook Fiction
              | NonfictionBook Nonfiction
              deriving Show

type AuthorName = String

data Author = Author (AuthorName, BookType)

-- construction and deconstruction

data GuessWhat = Chickenbutt deriving (Eq, Show)

data Id a = MkId a deriving (Eq, Show)

data Product a b = Product a b deriving (Eq, Show)

data Sum a b =
        First a
    |   Second b
    deriving (Eq, Show)

data RecordProduct a b = 
    RecordProduct {
        pfirst:: a,
        psecond:: b
    } deriving (Eq, Show)

newtype NumCow =
    NumCow Int
    deriving (Eq, Show)

newtype NumPig =
    NumPig Int
    deriving (Eq, Show)

data Farmhouse =
    Farmhouse NumCow NumPig
    deriving (Eq, Show)

type Farmhouse' = Product NumCow NumPig

newtype NumSheep = NumSheep Int deriving (Eq, Show)

data BigFarmhouse = BigFarmhouse NumCow NumPig NumSheep
    deriving (Eq, Show)

type BigFarmhouse' = Product NumCow (Product NumPig NumSheep)

type Name = String
type Age = Int
type LovesMud = Bool

type PoundsOfWool = Int

data CowInfo =
    CowInfo Name Age
    deriving (Eq, Show)

data PigInfo =
    PigInfo Name Age LovesMud
    deriving (Eq, Show)

data SheepInfo =
    SheepInfo Name Age PoundsOfWool
    deriving(Eq, Show)

data Animal =
      Cow CowInfo
    | Pig PigInfo
    | Sheep SheepInfo
    deriving (Eq, Show)

type Animal' = Sum CowInfo (Sum PigInfo SheepInfo)

bess' = CowInfo "Bess" 4
bess = First bess' :: Animal'

e' = Second (SheepInfo "Elmer" 5 5)
elmer = Second e' :: Animal'

elmo' = Second (SheepInfo "Elmo" 5 5)
-- The below produces and error
--elmo = First elmo' :: Animal'

sheep = SheepInfo "Baaaa" 5 5

trivialValue :: GuessWhat
trivialValue = Chickenbutt

idInt :: Id Integer
idInt = MkId 10

type Awesome = Bool
-- type Name = String

person :: Product Name Awesome
person = Product "Simon" True

data Twitter = Twitter deriving (Eq, Show)

data AskFm = AskFm deriving (Eq, Show)

socialNetwork ::  Sum Twitter AskFm
socialNetwork = First Twitter

newtype Name' = Name String deriving Show
newtype Acres = Acres Int deriving Show

data FarmerType = DairyFarmer
                | WheatFarmer
                | SoybeanFarmer
                deriving Show

data Farmer = Farmer Name Acres FarmerType
    deriving Show

isDairyFarmer :: Farmer -> Bool
isDairyFarmer (Farmer _ _ DairyFarmer) = True
isDairyFarmer _ = False

data FarmerRec =
    FarmerRec {
        names :: Name',
        acres :: Acres,
        farmerType :: FarmerType
    }
    deriving Show

isDairyFarmerRec :: FarmerRec -> Bool
isDairyFarmerRec farmer =
    case farmerType farmer of
        DairyFarmer -> True
        _ -> False

data BinaryTree a =
      Leaf
    | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a = Node left a right
  | b < a = Node (insert' b left) a right
  | b > a = Node left a (insert' b right)

t1 = insert' 0 Leaf
t2 = insert' 3 t1
t3 = insert' 5 t2

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) =
    Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' = Node (Node Leaf 3 Leaf)
            1
            (Node Leaf 4 Leaf)

mapExpected =
    Node (Node Leaf 4 Leaf)
         2
         (Node Leaf 5 Leaf)

mapOkay =
    if mapTree (+1) testTree' == mapExpected
    then print "yup okay"
    else error "test failed"

testTree :: BinaryTree Integer
testTree =
    Node (Node Leaf 1 Leaf)
    2
    (Node Leaf 3 Leaf)

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = a : preorder left ++ preorder right

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) = inorder left ++ [a] ++ inorder right

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) = postorder left ++ postorder right ++ [a]

testPreorder :: IO ()
testPreorder =
    if preorder testTree == [2,1,3]
    then putStrLn "Preorder find!"
    else putStrLn "bad news"

testInOrder :: IO ()
testInOrder =
    if inorder testTree == [1,2,3]
    then putStrLn "Inorder fine!"
    else putStrLn "bad news"

testPostorder :: IO ()
testPostorder =
    if postorder testTree == [1,3,2]
    then putStrLn "post order fine!"
    else putStrLn "post order failed"

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ z Leaf = z
foldTree f z (Node left a right) = f a (foldTree f (foldTree f z left) right)

main :: IO ()
main = do
    testPreorder
    testInOrder
    testPostorder
    print (foldTree (+) 0 testTree)