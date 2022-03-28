data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving Show

-- traverse :: (a -> f b) -> t a -> f (t b)

instance Functor Tree where
    fmap f EmptyTree           = EmptyTree
    fmap f (Node a left right) = Node (f a)  (f <$> left) (f <$> right)  

instance Foldable Tree where
    foldMap f EmptyTree = mempty 
    foldMap f (Node a left right) =
                            f a `mappend` foldMap f left `mappend` foldMap f right

instance Traversable Tree where
    traverse f EmptyTree           = pure EmptyTree
    traverse f (Node a left right) = Node <$> f a <*> traverse f left <*> traverse f right 

myTree :: Tree Int
myTree = Node 1
            (Node 2 
                (Node 4 EmptyTree EmptyTree)
                (Node 5 EmptyTree EmptyTree)
                )
            (Node 3
                (Node 6 EmptyTree EmptyTree)
                (Node 7 EmptyTree EmptyTree)
            )

main = do
    traverse print myTree
