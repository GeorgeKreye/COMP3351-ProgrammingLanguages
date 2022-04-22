{-# OPTIONS_GHC -Wno-type-defaults #-}
module TriTreeSpec where
    import Test.Hspec
    import TriTree

    spec :: Spec
    spec = do
        describe "search" $ do
            it "produces the boolean False" $
                search 1 Empty `shouldBe` False