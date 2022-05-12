module DailyHaskellEight where
    -- Event type
    data Event a = Event [Char] Int [Char] Int Double Double
        deriving (Eq, Show)
    {-
        Constructor utility for making events and making sure they are valid
        Takes all required values for an Event type - returns Nothing if invalid, Just(Event) otherwise
    -}
    makeEvent :: [Char] -> Int -> [Char] -> Int -> Double -> Double -> Maybe (Event a)
    makeEvent name day month year x y =
        let
            isLeapYear = (year `mod` 4 == 0) && (year `mod` 100 /= 0 || year `mod` 400 == 0)
        in
            if (((((day < 1 || day > 31) || (length month /= 3)) || (month /= "Jan" && month /= "Feb" && month /= "Mar" && month /= "Apr" && month /= "May" && month /= "Jun" && month /= "Jul" && month /= "Aug" && month /= "Sep" && month /= "Oct" && month /= "Nov" && month /= "Dec")) || ((month == "Apr" || month == "Jun" || month == "Sep" || month == "Nov") && day > 30)) || (month == "Feb" && isLeapYear  && day > 29)) || (month == "Feb" && not isLeapYear && day > 28)
                then Nothing
                else Just (Event name day month year x y)
    {- 
        Given a year (in the form of an integer) and a list of Events, returns a list
        containing only those events that occured in the given year
    -}
    inYear :: Int -> [Event a] -> [Event a]
    inYear y = filter (isYear y)
    -- helper function
    isYear :: Int -> Event a -> Bool
    isYear y (Event _ _ _ v _ _) = y == v
    {-
        Given a range of days (in the form of 2 integers marking the start and end
        of the range) and a list of Events, returns a list of the names of events
        that occured in the range; can be in any month or year
    -}
    inDayRange :: Foldable t => Int -> Int -> t (Event a) -> [[Char]]
    inDayRange s e = foldr (\(Event n d _ _ _ _) a -> 
        if d >= s && d <= e
            then n : a
            else a) []
