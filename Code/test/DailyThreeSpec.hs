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
        describe "countOccurences" $ do
            it "produces the integer 2" $
                countOccurences 'a' ['a','b','c','a'] `shouldBe` 2
            it "produces the integer 0" $
                countOccurences 1 [2,4,5,2] `shouldBe` 0
        describe "substitute" $ do
            it "produces the list [1,2,4,4]" $
                substitute 3 4 [1,2,3,4] `shouldBe` [1,2,4,4]
            it "producs the string \"hello\"" $
                substitute 'x' 'l' "hexxo" `shouldBe` "hello"
        describe "setListDiff" $ do 
            it "produces the list []" $
                setListDiff [3,4,5,6] [3,4,5,6] `shouldBe` []
            it "produces the list [9,2,7]" $
                setListDiff [9,8,6,5,4,3,2,1] [8,7,6,5,4,3,1] `shouldBe` [9,2,7]
