{-# OPTIONS_GHC -Wno-type-defaults #-}
module DailyTwoSpec where
    import Test.Hspec
    import DailyTwo

    spec :: Spec 
    spec = do
        describe "every4th" $ do
            it "produces the list [4]" $
                every4th [1,2,3,4] `shouldBe` [4]
            it "produces the list [4,8]" $
                every4th [1,2,3,4,5,6,7,8,9,10] `shouldBe` [4,8]
        describe "tupleDotQuotient" $ do 
            it "produces the dot product 2" $
                tupleDotQuotient [1,2] [1,2] `shouldBe` 2
            it "produces the dot product 349/140" $
                tupleDotQuotient [3,6,2,1] [5,4,8,7] `shouldBe` 349 / 140
        describe "appendToEach" $ do
            it "produces the list [\"hihi\"]" $
                appendToEach "hi" ["hi"] `shouldBe` ["hihi"]
            it "produces the list [\"no way\",\"yes way\"]" $
                appendToEach " way" ["no","yes"] `shouldBe` ["no way","yes way"]
        describe "toSetList" $ do
            it "produces the list [3,2,1]" $
                toSetList [3,2,1,2,1,3,2,3,1] `shouldBe` [3,2,1]
            it "produces the list [4,7,9,3,1,2,8,10,5,6]" $
                toSetList [4,7,9,3,1,2,8,10,5,3,9,10,3,1,4,8,6,1,10,6,5,4,8,5,2,7,2,7,6,9] `shouldBe` [4,7,9,3,1,2,8,10,5,6]
