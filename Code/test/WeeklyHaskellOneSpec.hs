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
        describe "removeWhitespace" $ do
            it "produces the string \"nosun\"" $
                removeWhitespace "no sun\n" `shouldBe` "nosun"
            it "produces the string \"carsandruckes\"" $
                removeWhitespace "cars and\trucke\rs" `shouldBe` "carsandruckes"
            it "produces the string \"\"" $
                removeWhitespace "\n \t \r" `shouldBe` ""
        describe "removePunctuation" $ do
            it "produces the string \"Hello My name is Joe\"" $
                removePunctuation "Hello! My name is Joe." `shouldBe` "Hello My name is Joe"
            it "produces the string \"Can I ask a question\"" $
                removePunctuation "(Can I ask a) question?" `shouldBe` "Can I ask a question"
            it "produces the string \"\"" $
                removePunctuation "Includes: pencils, paper, and an eraser" `shouldBe` "Includes pencils paper and an eraser"
