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
            it "produces the string \"\"" $
                removeChar 't' "" `shouldBe` ""
        describe "removeWhitespace" $ do
            it "produces the string \"nosun\"" $
                removeWhitespace "no sun\n" `shouldBe` "nosun"
            it "produces the string \"carsandruckes\"" $
                removeWhitespace "cars and\trucke\rs" `shouldBe` "carsandruckes"
            it "produces the string \"\"" $
                removeWhitespace "" `shouldBe` ""
        describe "removePunctuation" $ do
            it "produces the string \"Hello My name is Joe\"" $
                removePunctuation "Hello! My name is Joe." `shouldBe` "Hello My name is Joe"
            it "produces the string \"Can I ask a question\"" $
                removePunctuation "(Can I ask a) question?" `shouldBe` "Can I ask a question"
            it "produces the string \"\"" $
                removePunctuation "" `shouldBe` ""
        describe "charsToAscii" $ do
            it "produces the list [72, 101, 108, 108, 111, 32, 119, 111, 114, 108, 100]" $
                charsToAscii "Hello world" `shouldBe` [72, 101, 108, 108, 111, 32, 119, 111, 114, 108, 100]
            it "produces the list []" $
                charsToAscii "" `shouldBe` []
            it "produces the list [82, 97, 100, 105, 99, 97, 108, 33]" $
                charsToAscii "Radical!" `shouldBe` [82, 97, 100, 105, 99, 97, 108, 33]
        describe "asciiToChars" $ do
            it "produces the string \"Hello World\"" $
                asciiToChars [72, 101, 108, 108, 111, 32, 87, 111, 114, 108, 100] `shouldBe` "Hello World"
            it "produces the string \"\"" $
                asciiToChars [] `shouldBe` ""
            it "produces the string \"*(%$)@#!@^&*%(\"" $
                asciiToChars [42, 40, 37, 36, 41, 64, 35, 33, 64, 94, 38, 42, 37, 40] `shouldBe` "*(%$)@#!@^&*%("
        describe "shiftInts" $ do
            it "produces the list [2,3,4]" $
                shiftInts 1 [1,2,3,1277] `shouldBe` [2,3,4,0]
            it "produces the list []" $
                shiftInts 2 [] `shouldBe` []
            it "produces the list [1,2,3]" $
                shiftInts (-122) [123,124,125] `shouldBe` [1,2,3]
