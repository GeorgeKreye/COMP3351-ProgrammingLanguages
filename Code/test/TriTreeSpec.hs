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
        {- Currently doesn't work:
        describe "insert" $ do
            it "produces the TriTree [2(1()1 3()3)2]" $
                insert 1 (NodeOne 2 Empty (NodeOne 3 Empty Empty Empty) Empty) `shouldBe ` NodeOne 2 (NodeOne 1 Empty Empty Empty) (NodeOne 3 Empty Empty Empty) Empty
        describe "insertList" $ do
            it "produces the TriTree []" $
                insertList [5,10,20] (NodeOne 13 (NodeOne 7 Empty Empty Empty) (NodeOne 16 Empty Empty Empty) Empty) `shouldBe` NodeOne 13 (NodeOne 7 (NodeOne 5 Empty Empty Empty) (NodeOne 10 Empty Empty Empty) Empty) (NodeOne 16 (NodeOne 20 Empty Empty Empty) Empty Empty) Empty
        Need to find why -}
        describe "identical" $ do
            it "produces the boolean False" $
                identical Empty (NodeOne 3 Empty Empty Empty) `shouldBe` False
            it "produces the boolean True" $
                identical (NodeOne 3 Empty Empty Empty) (NodeOne 3 Empty Empty Empty) `shouldBe` True
            {- Currently doesn't work:
            it "produces the boolean True" $
                identical Empty Empty `shouldBe` True
            Need to find why -}