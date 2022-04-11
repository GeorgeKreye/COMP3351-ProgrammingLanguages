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
