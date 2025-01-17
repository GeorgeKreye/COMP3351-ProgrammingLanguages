{-# OPTIONS_GHC -Wno-type-defaults #-}
module DailyOneSpec where

import Test.Hspec
import DailyOne

spec :: Spec
spec = do
    describe "quadratic" $ do
        it "produces the quadratic 0" $
            quadratic 0 0 0 0 `shouldBe` 0
        
        it "produces the quadratic 3" $
            quadratic 1 1 1 1 `shouldBe` 3

    describe "scaleVector" $ do
        it "produces the scaled vector (1,1)" $
            scaleVector 1 (1,1) `shouldBe` (1,1)

        it "produces the scaled vector (2,2)" $
            scaleVector 2 (1,1) `shouldBe` (2,2)

    describe "tripleDistance" $ do
        it "produces the distance 0" $
            tripleDistance (3,9,5) (3,9,5) `shouldBe` 0

        it "produces the distance √3" $
            tripleDistance (1,1,1) (2,2,2) `shouldBe` 1.7320508075688772
