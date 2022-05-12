{-# OPTIONS_GHC -Wno-type-defaults #-}
module WeeklyHaskellThreeSpec where
    import Test.Hspec
    import WeeklyHaskellThree

    spec :: Spec
    spec = do
        describe "Vec - show" $ do
            it "produces the string \"Vec [1.0,2.0,3.0,4.0]\"" $
                show (Vec [1.0, 2.0, 3.0, 4.0]) `shouldBe` "Vec [1.0,2.0,3.0,4.0]"
            it "produces the string \"Vec []\"" $
                show (Vec []) `shouldBe` "Vec []"
        describe "Vec - (+)" $ do
            it "produces Vec []" $
                Vec [] + Vec [1.0,2.0] `shouldBe` Vec []
            it "produces Vec [2.0,3.0]" $
                Vec [1.0,2.0] + Vec [1.0,1.0] `shouldBe` Vec [2.0,3.0]
        describe "Vec - (*)" $ do
            it "produces Vec [4.0,9.0,16.0]" $
                Vec [2.0,3.0,4.0] * Vec [2.0,3.0,4.0] `shouldBe` Vec [4.0,9.0,16.0]
            it "produces Vec []" $
                Vec [] * Vec [1.0] `shouldBe` Vec []
        describe "Vec - abs" $ do
            it "produces Vec [1.0,2.0,3.0]" $
                abs (Vec [-1.0,-2.0,-3.0]) `shouldBe` Vec [1.0,2.0,3.0]
            it "produces Vec [4.0,5.0]" $
                abs (Vec [4.0,5.0]) `shouldBe` Vec [4.0,5.0]
            it "produces Vec []" $
                abs (Vec []) `shouldBe` Vec []
        describe "Vec - signum" $ do
            it "produces Vec []" $
                signum (Vec []) `shouldBe` Vec []
            it "produces Vec [-1.0,0.0,1.0]" $
                signum (Vec [-2.5,0,2.5]) `shouldBe` Vec[-1.0,0.0,1.0]
