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
