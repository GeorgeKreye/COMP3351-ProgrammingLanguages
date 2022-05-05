{-# OPTIONS_GHC -Wno-type-defaults #-}
module DailyHaskellSevenSpec where
    import Test.Hspec ( shouldBe, it, describe, Spec )
    import DailyHaskellSeven ( createOneList, findLargest, allTrue )

    spec :: Spec
    spec = do
        -- createOneList tests don't compile
        describe "createOneList" $ do
            it "produces the list [1,2,3,4,5]" $
                createOneList [[1,2],[3],[],[4,5]] `shouldBe` [1,2,3,4,5]
            it "priduces the list []" $
                createOneList [[],[],[]] `shouldBe` []
        describe "findLargest" $ do
            it "produces the integer 15" $
                findLargest [3,5,10,9,15] `shouldBe` 15
            it "produces the integer 0" $ 
                findLargest [] `shouldBe` 0
        describe "allTrue"$ do
            it "produces the boolean True" $
                allTrue [True, True, True] `shouldBe` True
            it "produces the boolean False" $
                allTrue [False, True] `shouldBe` False
            it "produces the boolean False" $
                allTrue [] `shouldBe` False
