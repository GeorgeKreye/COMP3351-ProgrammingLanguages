module DailyHaskellSeven where
    {- 
        Given a foldable list of lists, creates a single list containing all elements
        contained in the list of lists
        TODO: Find correct function
    -}
    createOneList :: Foldable t => t [[a]] -> [a]
    createOneList = foldr (\e a -> getList e ++ a) []
    -- helper function
    getList :: [[a]] -> [a]
    getList [[]] = []
    getList [] = []
    getList (x:xs) = x ++ getList xs
