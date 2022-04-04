{-# OPTIONS_GHC -Wno-type-defaults #-}
module DailyTwoSpec where
    import Test.Hspec
    import DailyTwo

    spec :: Spec 
    spec = do
        describe "every4th" $ do
            it "produces the list []" $
                every4th [1,2,3,4] `shouldBe` []
