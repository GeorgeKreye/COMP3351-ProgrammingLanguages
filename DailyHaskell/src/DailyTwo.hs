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
                if n `rem` 4 == 0
                    then every4thR lst (n+1) ++ [lst!!n]
                    else every4thR lst (n+1)
            else []
