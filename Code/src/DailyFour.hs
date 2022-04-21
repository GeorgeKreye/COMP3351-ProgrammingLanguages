{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use map" #-}
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
    firstValues (t:ts) = getFirstTriple t : firstValues ts
    secondValues :: [(a, b, c)] -> [b]
    secondValues [] = []
    secondValues (t:ts) = getSecondTriple t : secondValues ts
    thirdValues :: [(a,b,c)] -> [c]
    thirdValues [] = []
    thirdValues (t:ts) = getThirdTriple t : thirdValues ts
    {- 
        Takes 3 sorted lists and merges them in sorted order, returning
        the result.
    -}
    mergeSorted3 :: Ord a => [a] -> [a] -> [a] -> [a]
    mergeSorted3 [] [] [] = []
    mergeSorted3 (a:as) [] [] = a : mergeSorted3 as [] []
    mergeSorted3 [] (b:bs) [] = b : mergeSorted3 [] bs []
    mergeSorted3 [] [] (c:cs) = c : mergeSorted3 [] [] cs
    mergeSorted3 a b [] =
        if m == head a
            then head a : mergeSorted3 (tail a) b []
            else head b : mergeSorted3 a (tail b) []
        where m = minHead2 a b
    mergeSorted3 a [] c = 
        if m == head a
            then head a : mergeSorted3 (tail a) [] c
            else head c : mergeSorted3 a [] (tail c)
        where m = minHead2 a c
    mergeSorted3 [] b c =
        if m == head b
            then head b : mergeSorted3 [] (tail b) c
            else head c : mergeSorted3 [] b (tail c)
        where m = minHead2 b c
    mergeSorted3 a b c
      | m == head a = head a : mergeSorted3 (tail a) b c
      | m == head b = head b : mergeSorted3 a (tail b) c
      | otherwise = head c : mergeSorted3 a b (tail c)
      where m = minHead3 a b c
    --utility functions for mergeSorted3
    minHead2 :: Ord a => [a] -> [a] -> a
    minHead2 x y =
        if head x <= head y
            then head x
            else head y
    minHead3 :: Ord a => [a] -> [a] -> [a] -> a
    minHead3 a b c
      | head a <= head b && head a <= head c = head a
      | head b < head a && head b < head c = head b
      | otherwise = head c
