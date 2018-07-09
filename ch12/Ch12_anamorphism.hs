myIterate :: (a -> a) -> a -> [a]
myIterate f a = a : myIterate f (f a)

--take 10 $ unfoldr (\b -> Just (b, b+1)) 0
myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b = case f b of
                  Nothing -> []
                  Just (a, b') -> a : myUnfoldr f b'

betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\x -> Just (x, f x))

data BinaryTree a =
      Leaf
    | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Ord, Show)

-- unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
-- unfold f a = case f a of
--                 Nothing -> Leaf
--                 Just (a, b, c) -> Node (unfold f a) b (unfold f c)

unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f a =
  case f a of
    Nothing        -> Leaf
    Just (x, y, z) -> Node (unfold f x) y (unfold f z)

treeBuilder :: Integer -> BinaryTree Integer
treeBuilder n = unfold f 0
    where f x
            | x > n = Nothing
            | otherwise = Just (x+1, x, x+1)