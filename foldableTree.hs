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

testCheckA :: Int -> String 
testCheckA x = 
    if x == 5040
        then "Test A successfully produced 5040."
        else "Error occured in test A."

testCheckB :: Int -> String
testCheckB x = 
    if x == 28
        then "Test B successfully produced 28"
        else "Error occured in test B."
main = do
    print . testCheckA $ treeTestA myTree
    print . testCheckB $ treeTestB myTree
    
