{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use uncurry" #-}
module DailyHaskellFive where
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
