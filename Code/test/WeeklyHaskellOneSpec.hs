{-# OPTIONS_GHC -Wno-type-defaults #-}
module WeeklyHaskellOneSpec where
    import Test.Hspec
    import WeeklyHaskellOne
    
    spec :: Spec
    spec = do 
        describe "removeChar" $ do
            it "produces the string \"heo\"" $
                removeChar 'l' "hello" `shouldBe` "heo"
            it "produces the string \"Sea\"" $
                removeChar 's' "Seas" `shouldBe` "Sea"
            it "produces the string \"car\"" $
                removeChar 't' "catr" `shouldBe` "car"