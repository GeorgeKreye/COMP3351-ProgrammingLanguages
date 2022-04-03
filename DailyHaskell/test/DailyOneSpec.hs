-- file: Spec.hs
{-# OPTIONS_GHC -Wno-type-defaults #-}
module Main where

import Test.Hspec
import DailyOne

main :: IO ()
main = hspec $ do
    describe "quadratic" $
        it "produces the quadratic of 1, 1, 1, and 1" $
        quadratic 1 1 1 1 `shouldBe` 4

    describe "scaleVector" $
        it "produces the scaled vector of 2, (1, 1)" $
        (1 * 2, 1 * 2) `shouldBe` (2,2)

    describe "tripleDistance" $
        it "produces the distance between (1,1,1) and (2,2,2)" $
        sqrt((2 - 1) ^ 2 + (2 - 1) ^ 2 + (2 - 1) ^ 2) `shouldBe` 1.73205080757