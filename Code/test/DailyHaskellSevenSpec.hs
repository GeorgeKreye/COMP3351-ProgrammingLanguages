{-# OPTIONS_GHC -Wno-type-defaults #-}
module DailyHaskellSevenSpec where
    import Test.Hspec
    import DailyHaskellSeven

    spec :: Spec
    spec = do
        describe "createOneList" $ do
            it "produces the list [1,2,3,4,5]" $
                createOneList [[1,2],[3],[],[4,5]] `shouldBe` [1,2,3,4,5]
            it "priduces the list []" $
                createOneList [[],[],[]] `shouldBe` []
