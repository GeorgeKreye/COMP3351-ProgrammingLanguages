module DailyThree where
    import DailyTwo (toSetList)
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
    {- 
        Given two lists, returns the differences in unique elements
        between them.
        Call setListDiff, setListDiffL and setListDiffR are helper functions
    -}
    setListDiff :: Eq a => [a] -> [a] -> [a]
    setListDiff l1 l2 = setListDiffM (toSetList l1) (toSetList l2)
    setListDiffM :: Eq a => [a] -> [a] -> [a]
    setListDiffM l1 l2
      | null l1 && null l2 = []
      | not (null l1) && null l2 = l1
      | null l1 && not (null l2) = l2
      | otherwise = setListDiffL l1 l2 ++ setListDiffR l1 l2 ++ setListDiffM (tail l1) (tail l2)
    setListDiffL :: Eq a => [a] -> [a] -> [a]
    setListDiffL l1 l2
      | null l2 = [head l1]
      | head l1 == head l2 = []
      | otherwise = setListDiffL l1 (tail l2)
    setListDiffR :: Eq a => [a] -> [a] -> [a]
    setListDiffR l1 l2
      | null l1 = [head l2]
      | head l1 == head l2 = []
      | otherwise = setListDiffR (tail l1) l2
