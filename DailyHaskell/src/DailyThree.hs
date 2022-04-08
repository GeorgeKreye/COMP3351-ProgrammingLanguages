module DailyThree where
    {- 
        Given an element and a list, removes all elements that are not the
        given element and returns the resulting list
    -}
    removeAllExcept :: Eq a => a -> [a] -> [a]
    removeAllExcept e l
      | null l = []
      | head l == e = head l : removeAllExcept e (tail l)
      | otherwise = removeAllExcept e (tail l)
    {- 
        Given an element and a list, returns the count of occurences of
        that element in the list
    -}
    countOccurences :: Eq a => a -> [a] -> Int
    countOccurences e l
      | null l = 0
      | head l == e = countOccurences e (tail l) + 1
      | otherwise = countOccurences e (tail l)
    {- 
        Given a list and two elements, substitutes all instances of
        element a with element b and returns the resulting list
    -}
    substitute :: Eq a => a -> a -> [a] -> [a]
    substitute a b l
      | null l = []
      | head l == a = b : substitute a b (tail l)
      | otherwise = head l : substitute a b (tail l)
