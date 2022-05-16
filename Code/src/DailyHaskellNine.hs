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
    {- 
        Given (b -> c) and (a -> c) where Eq (f c) and f is a functor, returns
        True if the second functor law is upheld
    -}
    secondFunctorLaw :: (Eq (f c), Functor f) => (b -> c) -> (a -> b) -> f a -> Bool
    secondFunctorLaw f g functor = fmap (f . g) functor == fmap f (fmap g functor)
