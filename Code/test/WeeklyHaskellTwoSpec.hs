{-# OPTIONS_GHC -Wno-type-defaults #-}
module WeeklyHaskellTwoSpec where
    import Test.Hspec
    import WeeklyHaskellTwo

    spec :: Spec 
    spec = do
        describe "" $ do
            it "" $
               1+1 `shouldBe` 2
