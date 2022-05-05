{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use and" #-}
module DailyHaskellSeven where
    {- 
        Given a foldable list of lists, creates a single list containing all elements
        contained in the list of lists
        TODO: find function that works
    -}
    createOneList :: Foldable t => t [[a]] -> [a]
    createOneList = foldr _ [] -- function needed where hole is
    -- non-foldr implementation for reference
    getList :: [[a]] -> [a]
    getList [[]] = []
    getList [] = []
    getList (x:xs) = x ++ getList xs
    {- 
        Given a list of positive integers, returns the largest integer
    -}
    findLargest :: [Int] -> Int
    findLargest =
        foldr (\e p -> 
            if e > p
                then e
                else p
            ) 0
    {-
        Given a list of booleans, returns True if all booleans are true and false otherwise
    -}
    allTrue :: [Bool] -> Bool
    allTrue [] = False
    allTrue l = foldr (&&) True l
