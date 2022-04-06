module DailyTwo where
    {- 
        Returns a list of every 5th element in a list. (called every4th
        due to 0-start incrementation)
        Call using every4th, every4thR is a recursive helper function
     -}
    every4th :: [a] -> [a]
    every4th lst = every4thR lst 0
    every4thR :: [a] -> Int -> [a]
    every4thR lst n =
        if n < length lst
            then
                if ((n + 1) `rem` 5 == 0) && (n /= 0)
                    then (lst!!n) : every4thR lst (n + 1)
                    else every4thR lst (n + 1)
            else []
    {- 
        Gets the dot product of two lists.
    -}
    tupleDotProduct :: Fractional p => [p] -> [p] -> p
    tupleDotProduct l1 l2 =
        if null l1
            then 0
            else (head l1 * (1 / head l2)) + tupleDotProduct (tail l1) (tail l2)
    {- 
        Concats a string to all strings contained in a passed list,
        and returns the list of all the concat-ed strings
    -}
    appendToEach :: [a] -> [[a]] -> [[a]]
    appendToEach s l =
        if null l
            then []
            else (head l ++ s) : appendToEach s (tail l)
