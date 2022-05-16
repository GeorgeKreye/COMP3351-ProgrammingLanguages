{-# OPTIONS_GHC -Wno-type-defaults #-}
module DailyHaskellNineSpec where
    import Test.Hspec
    import DailyHaskellNine
    import Data.Char

    spec :: Spec
    spec = do 
        describe "firstFunctorLaw" $ do
            it "produces the boolean True" $
                firstFunctorLaw (Just ('c',35)) `shouldBe` True
            it "produces the boolean True" $
                firstFunctorLaw [2,3,5,7,11] `shouldBe` True
        describe "secondFunctorLaw" $ do
            it "produces the boolean True" $
                secondFunctorLaw isAlpha fst (Just('c',35)) `shouldBe` True
            it "produces the boolean True" $
                secondFunctorLaw chr (+96) [2,3,5,7,11] `shouldBe` True
