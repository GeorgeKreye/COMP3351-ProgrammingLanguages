-- file: Spec.hs
module Main where

import Test.Hspec
import DailyOne ( quadratic )

main :: IO ()
main = hspec $ do
    describe "quadratic" $ do
        it "produces the quadratic of 1, 1, 1, and 1" $
            quadratic 1 1 1 1 `shouldBe` 4

    describe "scaleVector" $ do
        it "produces the scaled vector of 2, (1, 1)" $
            scaleVector 2 (1,1) `shouldBe` (2,2)

    describe "tripleDistance" $ do
        it "produces the distance between (1,1,1) and (2,2,2)" $
            tripleDistance (1,1,1) (2,2,2) `shouldBe` 1.73205080757