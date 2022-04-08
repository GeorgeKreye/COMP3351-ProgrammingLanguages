{-# OPTIONS_GHC -Wno-type-defaults #-}
module DailyThreeSpec where
    import Test.Hspec
    import DailyThree

    spec :: Spec 
    spec = do
        describe "removeAllExcept" $ do
            it "produces the string \"aa\"" $
                removeAllExcept 'a' ['b','a','c','a'] `shouldBe` "aa"
            it "produces the list [1]" $
                removeAllExcept 1 [2,3,4,1] `shouldBe` [1]