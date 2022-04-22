{-# OPTIONS_GHC -Wno-type-defaults #-}
module DailyHaskellFiveSpec where
    import Test.Hspec
    import DailyHaskellFive
    spec :: Spec
    spec = do
        describe "multPairs" $ do
            it "produces the list []" $
                multPairs [] `shouldBe` []
            it "produces the list [15,81,48,32]" $
                multPairs [(3,5),(9,9),(6,8),(4,8)] `shouldBe` [15,81,48,32]
            it "produces the list [-20, 0]" $
                multPairs [(-4,5),(0,7)] `shouldBe` [-20, 0]
        describe "squareList" $ do
            it "produces the list []" $
                squareList [] `shouldBe` []
            it "produces the list [(1,1),(2,4),(3,9),(4,16)]" $
                squareList [1,2,3,4] `shouldBe` [(1,1),(2,4),(3,9),(4,16)]
            it "produces the list [(-1,1),(-2,4),(-3,9)]" $
                squareList [-1,-2,-3] `shouldBe` [(-1,1),(-2,4),(-3,9)]
        describe "findLowercase" $ do 
            it "produces the list []" $
                findLowercase [] `shouldBe` []
            it "produces the list [False,True,False]" $
                findLowercase ["Good", "afternoon", "Bob"] `shouldBe` [False,True,False]
            it "produces the list [False,False,True]" $
                findLowercase ["@","1","a"] `shouldBe` [False,False,True]
