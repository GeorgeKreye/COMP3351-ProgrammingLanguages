module DailyFour where
    {- 
        Given 3 lists, creates a list of tuples where each tuple contains
        the nth element of the list.
        Assumes all lists are the same size.
    -}
    zip3Lists :: [a1] -> [a2] -> [a3] -> [(a1, a2, a3)]
    zip3Lists a b c =
        if null a
            then []
            else (head a, head b, head c) : zip3Lists (tail a) (tail b) (tail c)
    {- 
        Given a list of triples, creates a tuple of three lists, containing
        the first elements, second elements, and third elements of each
        tuple respectively
    -}
    unzipTriples :: [(a,b,c)] -> ([a],[b],[c])
    unzipTriples t = (firstValues t, secondValues t, thirdValues t)
    -- utility functions for unzipTriples
    getFirstTriple :: (a,b,c) -> a
    getFirstTriple (a,_,_) = a
    getSecondTriple :: (a, b, c) -> b
    getSecondTriple (_,b,_) = b
    getThirdTriple :: (a, b, c) -> c
    getThirdTriple (_,_,c) = c
    firstValues :: [(a, b, c)] -> [a]
    firstValues [] = []
    firstValues t = getFirstTriple (head t) : firstValues (tail t)
    secondValues :: [(a, b, c)] -> [b]
    secondValues [] = []
    secondValues t = getSecondTriple (head t) : secondValues (tail t)
    thirdValues :: [(a,b,c)] -> [c]
    thirdValues [] = []
    thirdValues t = getThirdTriple (head t) : thirdValues (tail t)
    {- 
        Takes 3 sorted lists and merges them in sorted order, returning
        the result.
    -}
    mergeSorted3 :: Ord a => [a] -> [a] -> [a] -> [a]
    mergeSorted3 [] [] [] = []
    mergeSorted3 a [] [] = a
    mergeSorted3 [] b [] = b
    mergeSorted3 [] [] c = c
