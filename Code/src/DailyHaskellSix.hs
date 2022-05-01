module DailyHaskellSix where
    {- 
        Given an integer and a list of words, filters said list to include only words that are
        shorten than or equal to the given integer in length
    -}
    shorterThan :: Int -> [[Char]] -> [[Char]]
    shorterThan l = filter ((<= l) . length)