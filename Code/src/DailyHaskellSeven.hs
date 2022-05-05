{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use and" #-}
{-# HLINT ignore "Use concat" #-}
module DailyHaskellSeven where
    {- 
        Given a foldable list of lists, creates a single list containing all elements
        contained in the list of lists
        TODO: find function that works
    -}
    createOneList :: [[a]] -> [a]
    createOneList = foldr (++) []
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
