module DailyThree where
    removeAllExcept :: Eq a => a -> [a] -> [a]
    removeAllExcept e l
      | null l = []
      | head l == e = head l : removeAllExcept e (tail l)
      | otherwise = removeAllExcept e (tail l)