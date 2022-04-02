--  Question 15

expr :: (Monad m) => m (m a) -> m a
expr = \ mm -> do {
        m <- mm ; m
    }

-- Expanded
-- (1) \mm -> do {
--         m <- mm ; m
--     }
-- (2) \mm -> (mm >>= (\m -> do m))
--     (\m -> do m) can be replaced with the identity function since its just outputting the input
-- (3) \mm -> (mm >>= id)

-- Question 16

myUncurry :: (a -> b -> c) -> (a,b) -> c
myUncurry f (a,b) = f a b

-- This will take a multi-argument function and return the same function with one argument, 
-- along with the remaining arguments

someFunc :: Int -> Int -> Int 
someFunc a b = a + b

    