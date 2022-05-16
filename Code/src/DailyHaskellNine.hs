{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module DailyHaskellNine where
    {- TODO: write -}
    firstFunctorLaw :: (Eq (f a), Functor f) => f a -> Bool
    firstFunctorLaw f = True