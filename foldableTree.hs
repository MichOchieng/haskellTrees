data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving Show

instance Foldable Tree where
    foldMap f EmptyTree = mempty 
    foldMap f (Node a left right) =
                            f a `mappend` foldMap f left `mappend` foldMap f right

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

treeTestA :: Tree Int -> Int 
treeTestA = foldl (\acc x -> x * acc) 1

treeTestB :: Tree Int -> Int
treeTestB = foldl (\acc x -> x + acc) 0

main = do
    print $ treeTestA myTree
    print $ treeTestB myTree
    
