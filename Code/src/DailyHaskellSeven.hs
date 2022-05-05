{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use and" #-}
module DailyHaskellSeven where
    {- 
        Given a foldable list of lists, creates a single list containing all elements
        contained in the list of lists
        TODO: fix
    -}
    createOneList :: Foldable t => t [[a]] -> [a]
    createOneList = foldr (\e a -> getList e ++ a) []
    -- helper function
    getList :: [[a]] -> [a]
    getList [[]] = []
    getList [] = []
    getList (x:xs) = x ++ getList xs -- might be incorrect
    {- 
        Given a list of positive integers, returns the largest integer
    -}
    findLargest :: [Int] -> Int
    findLargest =
        {- non-fold:
        let
            prev = findLargest xs
        in
            if x > prev
                then x
                else prev
        -}
        -- fold:
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
