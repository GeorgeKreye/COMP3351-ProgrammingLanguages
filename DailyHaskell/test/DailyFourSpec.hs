{-# OPTIONS_GHC -Wno-type-defaults #-}
module DailyFourSpec where
    import Test.Hspec
    import DailyFour

    spec :: Spec 
    spec = do
        describe "zip3Lists" $ do
            it "produces the list [(1,'a',4),(2,'b',5),(3,'c',6)]" $
                zip3Lists [1,2,3] ['a','b','c'] [4,5,6] `shouldBe` [(1,'a',4),(2,'b',5),(3,'c',6)]
            it "produces the list [(10,'v',0),(15,'w',1),(20,'x',0),(25,'y',0),(30,'z',1)]" $
                zip3Lists [10,15,20,25,30] ['v','w','x','y','z'] [0,1,0,0,1] `shouldBe` [(10,'v',0),(15,'w',1),(20,'x',0),(25,'y',0),(30,'z',1)]
