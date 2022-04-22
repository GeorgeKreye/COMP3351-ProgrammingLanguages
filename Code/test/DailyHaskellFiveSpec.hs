{-# OPTIONS_GHC -Wno-type-defaults #-}
module DailyHaskellFiveSpec where
    import Test.Hspec
    import DailyHaskellFive
    spec :: Spec
    spec = do
        describe "multPairs" $ do
            it "produces the list []" $
                multPairs [] `shouldBe` []
            it "produces the list [15, 81, 48, 32]" $
                multPairs [(3,5),(9,9),(6,8),(4,8)] `shouldBe` [15, 81, 48, 32]
            it "produces the list [-20, 0]" $
                multPairs [(-4,5),(0,7)] `shouldBe` [-20, 0]
