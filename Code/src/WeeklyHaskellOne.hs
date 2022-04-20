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
    {- 
        Removes all whitespace from a given string and returns the
        resulting string
    -}
    removeWhitespace :: [Char] -> [Char]
    removeWhitespace s = removeChar ' ' (removeChar '\t' (removeChar '\n' (removeChar '\r' s)))
    {- 
        Removes all periods, exclamation points, commas, etc from a given
        string, returning the result
    -}
    removePunctuation :: [Char] -> [Char]
    removePunctuation s = 
        removeChar '.' (removeChar '!' (removeChar ',' (removeChar '?' (removeChar ':' (removeChar ';' (removeChar '(' (removeChar ')' s)))))))
    {- 
        Converts a string to a list of integers representing the ASCII
        values of the characters that compose it
    -}
    charsToAscii :: Enum a => [a] -> [Int]
    charsToAscii [] = []
    charsToAscii s = fromEnum (head s) : charsToAscii(tail s)
    {- -}
