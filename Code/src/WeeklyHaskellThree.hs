{-# LANGUAGE DataKinds #-}
module WeeklyHaskellThree where
    -- vec type
    newtype Vec a =
        Vec3 [Double,Double,Double]
        deriving (Eq,Ord,Show)
