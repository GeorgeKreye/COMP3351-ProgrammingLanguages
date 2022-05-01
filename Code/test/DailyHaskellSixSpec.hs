{-# OPTIONS_GHC -Wno-type-defaults #-}
module DailyHaskellSixSpec where
    import Test.Hspec
    import DailyHaskellSix

    spec :: Spec
    spec = do
        describe "shorterThan" $ do
            it "produces the list []" $
                shorterThan 0 [] `shouldBe` []
            it "produces the list []" $
                shorterThan 4 ["Hello","Hi","Hey","Salutations"] `shouldBe` ["Hi","Hey"]