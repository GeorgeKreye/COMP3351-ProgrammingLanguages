{-# LANGUAGE DataKinds #-}
module WeeklyHaskellThree where
    class (Show a, Num a) => Vec a where
        (+) :: [a] -> [a] -> [a]
        (-) :: [a] -> [a] -> [a]
        (*) :: [a] -> [a] -> [a]
        (/) :: [a] -> [a] -> [a]
    instance Vec Double where
        (+) a b = zipWith (Prelude.+) a b
        (-) a b = zipWith (Prelude.-) a b
        (*) a b = zipWith (Prelude.*) a b
        (/) a b = zipWith (Prelude./) a b
