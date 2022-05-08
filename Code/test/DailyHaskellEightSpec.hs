{-# OPTIONS_GHC -Wno-type-defaults #-}
module DailyHaskellEightSpec where
    import Test.Hspec
    import DailyHaskellEight

    spec :: Spec
    spec = do
        describe "" $ do
            it "produces Nothing" $
                makeEvent "test" 1 "zeb" 0 0.0 0.0 `shouldBe` Nothing