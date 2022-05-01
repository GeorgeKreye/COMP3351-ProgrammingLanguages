{-# OPTIONS_GHC -Wno-type-defaults #-}
module DailyHaskellSixSpec where
    import Test.Hspec
    import DailyHaskellSix

    spec :: Spec
    spec = do
        describe "shorterThan" $ do
            it "produces the list []" $
                shorterThan 0 [] `shouldBe` []
            it "produces the list [\"Hi\",\"Hey\"]" $
                shorterThan 4 ["Hello","Hi","Hey","Salutations"] `shouldBe` ["Hi","Hey"]
        describe "removeMultiples" $ do
            it "produces the list []" $
                removeMultiples 1 [] `shouldBe` []
            it "produces the list [3,9]" $
                removeMultiples 5 [3,5,10,9,15] `shouldBe` [3,9]
        describe "onlyJust" $ do
            it "produces the list []" $
                onlyJust ([] :: [Maybe Int]) `shouldBe` []
            it "produces the list [Just 5, Just 10]" $
                onlyJust [Nothing, Just 5, Nothing, Just 10] `shouldBe` [Just 5, Just 10]
            it "produces the list []" $
                onlyJust ([Nothing, Nothing] :: [Maybe Int]) `shouldBe` []
        describe "allAnswers" $ do
            it "produces Nothing" $
                allAnswers invert [0,1] `shouldBe` Nothing
            it "produces the list [0.5,0.25,0.125]" $
                allAnswers invert [2,4,8] `shouldBe` Just [0.5,0.25,0.125]
            it "produces the list []" $
                allAnswers invert [] `shouldBe` Just []

