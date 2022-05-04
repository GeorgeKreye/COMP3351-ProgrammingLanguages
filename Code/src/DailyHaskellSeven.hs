{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use concat" #-}
{-# HLINT ignore "Evaluate" #-}
{-# HLINT ignore "Avoid lambda" #-}
module DailyHaskellSeven where
    {- 
        Given a foldable list of lists, creates a single list containing all elements
        contained in the list of lists
        TODO: Find correct function
    -}
    createOneList :: Foldable t => t [[a]] -> [a]
    createOneList = foldr foo []
    -- helper function
    foo :: [[a]] -> [a] -> [a]
    foo [] = _
    foo (x:xs) = _
