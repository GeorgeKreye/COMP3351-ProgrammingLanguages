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
        describe "insert" $
            it "produces the TriTree [2(1()1 3()3)2]" $
                insert 1 (NodeOne 2 Empty (NodeOne 3 Empty Empty Empty) Empty) `shouldBe ` (NodeOne 2 (NodeOne 1 Empty Empty Empty) (NodeOne 3 Empty Empty Empty) Empty)
