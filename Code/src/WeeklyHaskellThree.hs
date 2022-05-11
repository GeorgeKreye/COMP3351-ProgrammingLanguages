{-# LANGUAGE DataKinds #-}
module WeeklyHaskellThree where
    newtype Vec = Vec [Double]
    instance Num Vec where
      (+) (Vec a) (Vec b) = Vec (zipWith (+) a b)
      (*) (Vec a) (Vec b) = Vec (zipWith (*) a b)
      abs (Vec a)= Vec (map abs a)
      signum (Vec a)= Vec (map signum a)
      fromInteger i = Vec [fromInteger i]
      negate (Vec a) = Vec (map negate a)
    instance Show Vec where
      show (Vec a) = "Vec " ++ show a
    instance Eq Vec where
      (==) (Vec a) (Vec b) = and (zipWith (==) a b)
        
    -- helper function

