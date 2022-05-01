module DailyHaskellSix where
    import Data.Maybe (isJust, fromMaybe, isNothing)
    {- 
        Given an integer and a list of words, filters said list to include only words that are
        shorten than or equal to the given integer in length
    -}
    shorterThan :: Int -> [[Char]] -> [[Char]]
    shorterThan l = filter ((<= l) . length)
    {- 
        Given an integer and a list of integers, filters said list to remove all multiples of
        the given integer
    -}
    removeMultiples :: Int -> [Int] -> [Int]
    removeMultiples f = filter (\x -> x `mod` f /= 0)
    {-
        Given a list of Maybe instances, filters said list to remove any values of Nothing 
    -}
    onlyJust :: [Maybe a] -> [Maybe a]
    onlyJust = filter isJust
    {- 
        Given a function of type (a -> Maybe b) and a list of type [a], calls the function on each element of the list
        If the result of the function being called is Nothing, return Nothing; otherwise return a list of results wrapped by Just
    -}
    allAnswers :: (a -> Maybe b) -> [a] -> Maybe [b]
    allAnswers _ [] = Just []
    allAnswers f (x:xs) =
        let r = f x
            p = allAnswers f xs
            pV = fromMaybe [] p
        in
            if isNothing r || isNothing p
                then Nothing
                else Just (fromMaybe (head pV) r : pV) -- (head pV) should never be used, just a type placeholder
    -- utility function for testing
    invert :: (Eq a, Fractional a) => a -> Maybe a
    invert d =
        if d == 0
            then Nothing
            else Just (1 / d)
