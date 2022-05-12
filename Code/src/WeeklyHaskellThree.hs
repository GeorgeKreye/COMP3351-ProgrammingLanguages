{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module WeeklyHaskellThree where
    {-
        Vector type and associated instances/functions.
    -}
    newtype Vec = Vec [Double]
    -- Num instance implementation
    instance Num Vec where
      (+) (Vec a) (Vec b) = Vec (zipWith (+) a b)
      (*) (Vec a) (Vec b) = Vec (zipWith (*) a b)
      abs (Vec a) = Vec (map abs a)
      signum (Vec a) = Vec (map signum a)
      fromInteger i = Vec [fromInteger i]
      negate (Vec a) = Vec (map negate a)
    -- Show instance implementation
    instance Show Vec where
      show (Vec a) = "Vec " ++ show a
    -- Eq instance implementation
    instance Eq Vec where
      (==) (Vec a) (Vec b) = and (zipWith (==) a b)
    -- Ord instance implementation
    instance Ord Vec where
      compare (Vec a) (Vec b) = compare a b 
    -- Typeclass for magnitude
    class VecT a where
      magnitude :: VecT a => a -> Double
    -- VecT instance implementation, using head as magnitude
    instance VecT Vec where
      magnitude (Vec a) = head a
