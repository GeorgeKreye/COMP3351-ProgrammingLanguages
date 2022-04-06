{-# OPTIONS_GHC -Wno-type-defaults #-}
module DailyTwoSpec where
    import Test.Hspec
    import DailyTwo

    spec :: Spec 
    spec = do
        describe "every4th" $ do
            it "produces the list []" $
                every4th [1,2,3,4] `shouldBe` []
            it "produces the list [5,10]" $
                every4th [1,2,3,4,5,6,7,8,9,10] `shouldBe` [5,10]
        describe "tupleDotProduct" $ do 
            it "produces the dot product 2" $
                tupleDotProduct [1,2] [1,2] `shouldBe` 2
            it "produces the dot product 349/140" $
                tupleDotProduct [3,6,2,1] [5,4,8,7] `shouldBe` 349 / 140
        describe "appendToEach" $ do
            it "produces the list [\"hihi\"]" $
                appendToEach "hi" ["hi"] `shouldBe` ["hihi"]
            it "produces the list [\"no way\",\"yes way\"]" $
                appendToEach " way" ["no","yes"] `shouldBe` ["no way","yes way"]
