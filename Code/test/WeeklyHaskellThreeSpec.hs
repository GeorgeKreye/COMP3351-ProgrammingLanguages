{-# OPTIONS_GHC -Wno-type-defaults #-}
module WeeklyHaskellThreeSpec where
    import Test.Hspec
    import WeeklyHaskellThree

    spec :: Spec
    spec = do
        describe "Vec3 - Show" $ do
            it "produces the string \"Vec [1.0,2.0,3.0,4.0]\"" $
                show (Vec [1.0, 2.0, 3.0, 4.0]) `shouldBe` "Vec [1.0,2.0,3.0,4.0]"
            it "produces the string \"Vec []\"" $
                show (Vec []) `shouldBe` "Vec []"
        describe "Vec3 - (+)" $ do
            it "produces Vec []" $
                Vec [] + Vec [1.0,2.0] `shouldBe` Vec []
            it "produces Vec [2.0,3.0]" $
                Vec [1.0,2.0] + Vec [1.0,1.0] `shouldBe` Vec [2.0,3.0]    
