{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-imports #-} -- until issue solved
module WeeklyHaskellThreeSpec where
    import Test.Hspec
    import WeeklyHaskellThree

    spec :: Spec
    spec = do
        describe "Vec3-Show" $ do
            it "produces the string \"Vec [1.0, 2.0, 3.0, 4.0]\"" $
                print (Vec [1.0, 2.0, 3.0, 4.0]) `shouldBe` "Vec [1.0, 2.0, 3.0, 4.0]"
