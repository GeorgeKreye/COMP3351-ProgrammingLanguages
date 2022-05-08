{-# OPTIONS_GHC -Wno-type-defaults #-}
module DailyHaskellEightSpec where
    import Test.Hspec
    import DailyHaskellEight

    spec :: Spec
    spec = do
        describe "Event - Construction" $ do
            it "produces Nothing (invalid)" $
                makeEvent "test" 1 "zeb" 0 0.0 0.0 `shouldBe` Nothing
            it "produces Event \"test\" 20 \"Feb\" 1982 32.5 10.2 successfully" $
                makeEvent "test" 20 "Feb" 1982 32.5 10.2 `shouldBe` Just (Event "test" 20 "Feb" 1982 32.5 10.2)
