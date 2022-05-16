{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Functor law" #-}
{-# HLINT ignore "Redundant id" #-}
module DailyHaskellNine where
    {- 
        Given Eq (f a) where f is a functor, returns True if
        the first functor law is upheld
    -}
    firstFunctorLaw :: (Eq (f a), Functor f) => f a -> Bool
    firstFunctorLaw f = fmap id f == id f
