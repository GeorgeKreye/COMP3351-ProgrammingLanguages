module DailyOne where
    -- calculates a quadratic function of the form a + b * x + c^2 * x  
    quadratic :: Num a => a -> a -> a -> a -> a
    quadratic a b c x = a + (b * x) + ((c ^ (2 :: Integer)) * x)

    -- takes a number X and a vector V (represented by a 2-tuple (V1,V2))
    -- and creates a vector V' multiplied as (V1*X,V2*X)
    scaleVector :: Num b => b -> (b, b) -> (b, b)
    scaleVector x (a, b) = (a * x, b * x)

    -- finds the distance between two 3-dimensional points
    -- (represented as 3-tuples)
    tripleDistance :: Floating a => (a, a, a) -> (a, a, a) -> a
    tripleDistance (a,b,c) (d,e,f) = sqrt((d - a) ^ (2 :: Integer) + (e - b) ^ (2 :: Integer) + (f - c) ^ (2:: Integer))
