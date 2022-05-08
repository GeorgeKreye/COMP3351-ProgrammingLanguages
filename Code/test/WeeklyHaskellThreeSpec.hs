{-# OPTIONS_GHC -Wno-type-defaults #-}
module WeeklyHaskellThreeSpec where
    import Test.Hspec
    import WeeklyHaskellThree

    spec :: Spec
    spec = do
        describe "Vec3-Show" $ do
            it "produces the string \"Vec3 [1.0, 2.0, 3.0]\"" $
                print (Vec3 [1.0, 2.0, 3.0]) `shouldBe` "Vec3 [1.0, 2.0, 3.0]"