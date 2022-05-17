{-# OPTIONS_GHC -Wno-type-defaults #-}

module DailyHaskellNineSpec where
    import Test.Hspec
    import DailyHaskellNine
    import Data.Char
    import Data.Maybe

    spec :: Spec
    spec = do
        describe "firstFunctorLaw" $ do
            it "produces the boolean True" $
                firstFunctorLaw (Just ('c',35)) `shouldBe` True
            it "produces the boolean True" $
                firstFunctorLaw [2,3,5,7,11] `shouldBe` True
        describe "secondFunctorLaw" $ do
            it "produces the boolean True" $
                secondFunctorLaw isAlpha fst (Just ('c',35)) `shouldBe` True
            it "produces the boolean True" $
                secondFunctorLaw chr (+96) [2,3,5,7,11] `shouldBe` True
        describe "Either String (Maybe Integer) - firstFunctorLaw" $ do
            it "produces the boolean True" $
                firstFunctorLaw (Left "foo" :: Either String (Maybe Int)) `shouldBe` True
            it "produces the boolean True" $
                firstFunctorLaw (Right (Just 1) :: Either String (Maybe Int)) `shouldBe` True
            it "produces the boolean True" $
                firstFunctorLaw (Right Nothing :: Either String (Maybe Int)) `shouldBe` True
        describe "Either String (Maybe Integer) - secondFunctorLaw" $ do
            it "produces the boolean True" $
                secondFunctorLaw (+1) (fromMaybe 0) (Left "foo" :: Either String (Maybe Int))`shouldBe` True
            it "produces the boolean True" $
                secondFunctorLaw (+1) (fromMaybe 0) (Right (Just 1) :: Either String (Maybe Int)) `shouldBe` True
            it "produces the boolean True" $
                secondFunctorLaw (+1) (fromMaybe 0) (Right Nothing :: Either String (Maybe Int)) `shouldBe` True
