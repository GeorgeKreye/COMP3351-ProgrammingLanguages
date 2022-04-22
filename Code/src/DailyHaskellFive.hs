{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use uncurry" #-}
module DailyHaskellFive where
    import Data.Char (isLower)
    {-
        Given a list of pairs of integers, returns a list of the
        products of each pair
    -}
    multPairs :: [(Integer,Integer)] -> [Integer]
    multPairs = map (\(x,y) -> x * y)
    {- 
        Given a list of integers, returns a list of pairs consisting of
        the original integer and its square
    -}
    squareList :: [Integer] -> [(Integer, Integer)]
    squareList = map (\x -> (x, x * x))
    {- 
        Given a list of strings, returns a list of booleans that depends
        on whether the first character of each string is lowercase or not
    -}
    findLowercase :: [[Char]] -> [Bool]
    findLowercase = map (isLower . head)
    {- 
        Given a list of doubles, negates each double, takes the cos
        and tan of the negation, and returns the result
    -}
    negCosTan :: [Double] -> [Double]
    negCosTan = map (tan . cos . negate)
