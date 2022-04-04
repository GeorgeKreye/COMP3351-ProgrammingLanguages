{-# OPTIONS_GHC -Wno-type-defaults #-}
module DailyTwoSpec where
    import Test.Hspec
    import DailyTwo

    spec :: Spec 
    spec = do
        describe "placeholder" $ do
            it "placeholder" $
                placeholder `shouldBe` 2
                