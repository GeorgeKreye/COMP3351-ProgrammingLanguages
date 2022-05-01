module DailyHaskellSix where
    {- 
        Given an integer and a list of words, filters said list to include only words that are
        shorten than or equal to the given integer in length
    -}
    shorterThan :: Int -> [[Char]] -> [[Char]]
    shorterThan l = filter ((<= l) . length)
    {- 
        Given an integer and a list of integers, filters said list to remove all multiples of
        the given integer
    -}
    removeMultiples :: Int -> [Int] -> [Int]
    removeMultiples f = filter (\x -> x `mod` f /= 0)
