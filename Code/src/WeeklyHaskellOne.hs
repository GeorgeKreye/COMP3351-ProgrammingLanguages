module WeeklyHaskellOne where
    removeChar :: Eq a => a -> [a] -> [a]
    removeChar c s
      | null s = []
      | head s == c = removeChar c (tail s)
      | otherwise = head s : removeChar c (tail s)
    
