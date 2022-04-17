module WeeklyHaskellOne where
    {- 
        Given a char and a string, removes all instances of the given char
        and returns a string
    -}
    removeChar :: Eq a => a -> [a] -> [a]
    removeChar c s
      | null s = []
      | head s == c = removeChar c (tail s)
      | otherwise = head s : removeChar c (tail s)
