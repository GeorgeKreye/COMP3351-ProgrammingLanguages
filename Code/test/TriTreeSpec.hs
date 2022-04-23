{-# OPTIONS_GHC -Wno-type-defaults #-}
module TriTreeSpec where
    import Test.Hspec
    import TriTree

    spec :: Spec
    spec = do
        describe "search" $ do
        it "produces the boolean False" $
            search 1 Empty `shouldBe` False
        it "produces the boolean True" $
            search 5 (NodeOne 2 (NodeTwo 5 9 (NodeOne 7 Empty Empty Empty) (NodeOne 4 Empty Empty Empty) Empty) (NodeOne 3 Empty Empty Empty) Empty) `shouldBe` True
        it "produces the boolean False" $
            search 80 (NodeOne 2 (NodeTwo 5 9 (NodeOne 7 Empty Empty Empty) (NodeOne 4 Empty Empty Empty) Empty) (NodeOne 3 Empty Empty Empty) Empty) `shouldBe` False
