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
                signum (Vec [-2.5,0,2.5]) `shouldBe` Vec [-1.0,0.0,1.0]
        describe "Vec - fromInteger" $ do
            it "produces Vec [1.0]" $
                1 `shouldBe` Vec [1.0]
            it "produces Vec [-8.0]" $
                -8 `shouldBe` Vec [-8.0]
        describe "Vec - negate" $ do
            it "produces Vec [-3.5,4.8]" $
                - (Vec [3.5,-4.8]) `shouldBe` Vec [-3.5,4.8]
            it "produces Vec [0.0]" $
                - (Vec [0.0]) `shouldBe` Vec [0.0]
            it "produces Vec []" $
                - (Vec []) `shouldBe` Vec []
        describe "Vec - (==)" $ do
            it "produces the boolean True" $
                Vec [3.2,4.5,7.9] == Vec [3.2,4.5,7.9] `shouldBe` True
            it "produces the boolean False" $
                Vec [1.3,5.6] == Vec [5.6] `shouldBe` False
        describe "Vec - (<=)" $ do
            it "produces the boolean True" $
                Vec [3.7,9.5] <= Vec [3.7,10.3] `shouldBe` True
            it "produces the boolean False" $
                Vec [10.5,13.2] <= Vec [1.3] `shouldBe` False
            it "produces the boolean True" $
                Vec [] <= Vec [] `shouldBe` True
