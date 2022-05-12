{-# OPTIONS_GHC -Wno-type-defaults #-}
module DailyHaskellEightSpec where
    import Test.Hspec
    import DailyHaskellEight
    
    spec :: Spec
    spec = do
        describe "Event - Construction" $ do
            it "produces Nothing (invalid)" $
                makeEvent "test" 1 "zeb" 0 0.0 0.0 `shouldBe` Nothing
            it "produces Just (Event \"test\" 20 \"Feb\" 1982 32.5 10.2)" $
                makeEvent "test" 20 "Feb" 1982 32.5 10.2 `shouldBe` Just (Event "test" 20 "Feb" 1982 32.5 10.2)
        describe "inYear" $ do
            it "produces the list []" $
                inYear 2001 [] `shouldBe` []
            it "produces the list [Event \"Test1\" 5 \"Mar\" 2015 3.2 4.5, Event \"Test3\" 7 \"Dec\" 2015 56.3 89.5]" $
                inYear 2015 [Event "Test1" 5 "Mar" 2015 3.2 4.5, Event "Test2" 15 "Feb" 1980 32.5 0.0, Event "Test3" 7 "Dec" 2015 56.3 89.5] `shouldBe` [Event "Test1" 5 "Mar" 2015 3.2 4.5, Event "Test3" 7 "Dec" 2015 56.3 89.5]
        describe "inDayRange" $ do
            it "produces the list []" $
                inDayRange 4 12 [] `shouldBe` []
            it "produces the list [\"test2\",\"test4\"]" $
                inDayRange 1 7 [Event "test1" 9 "Jan" 1920 45.2 10.7,Event "test2" 3 "Sep" 2020 45.0 73.15,Event "test3" 26 "Jun" 1350 0.0 15.5,Event "test4" 7 "Aug" 2007 12.3 16.5] `shouldBe` ["test2","test4"]
