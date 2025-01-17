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
            it "produces the list [(2,3,4),(4,6,8),(6,9,12)]" $
                zip3Lists [2,4,6] [3,6,9] [4,8,12] `shouldBe` [(2,3,4),(4,6,8),(6,9,12)]
        describe "unzipTriples" $ do
            it "produces the tuple ([1,4,7],[2,5,8],[3,6,9])" $
                unzipTriples [(1,2,3),(4,5,6),(7,8,9)] `shouldBe` ([1,4,7],[2,5,8],[3,6,9])
            it "produces the tuple ([1,4,7,10,13],[2,5,8,11,14],[3,6,9,12,15])" $
                unzipTriples [(1,2,3),(4,5,6),(7,8,9),(10,11,12),(13,14,15)] `shouldBe` ([1,4,7,10,13],[2,5,8,11,14],[3,6,9,12,15])
            it "produces the tuple (\"a1x7\",\"b2y8\",\"c3z9\")" $
                unzipTriples [('a','b','c'),('1','2','3'),('x','y','z'),('7','8','9')] `shouldBe` ("a1x7","b2y8","c3z9")
        describe "mergeSorted3" $ do
            it "produces the list [-1, 0, 1, 2, 3, 4, 5, 8, 10]" $
                mergeSorted3 [2, 3, 5] [1, 8] [-1, 0, 4, 10] `shouldBe`[-1, 0, 1, 2, 3, 4, 5, 8, 10]
            it "produces the list [5,11,12,13,17,19,19,21,22,31,32,33,43,45,48,57,59,64]" $
                mergeSorted3 [11,12,13,19] [5,19,21,31,32,33,43,48] [17,22,45,57,59,64] `shouldBe` [5,11,12,13,17,19,19,21,22,31,32,33,43,45,48,57,59,64]
            it "produces the list []" $
                mergeSorted3 [234,325,546] [635,5624] [576,2356,3625,4563] `shouldBe` [234,325,546,576,635,2356,3625,4563,5624]
