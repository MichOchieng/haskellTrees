module Main where


test :: (Monad m) => m a -> m a
test = \ mm -> do {
        m <- mm ; return m
    }

-- test mm = do {
--         m <- mm ; m
--     }

main = do
    print $ test [1]
    print "hi"