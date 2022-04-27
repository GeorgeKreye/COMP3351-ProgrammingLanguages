{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
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
        describe "insert" $ do
            it "produces the TriTree [1()1]" $
                insert (1 :: Int) Empty `shouldBe` NodeOne (1 :: Int) Empty Empty Empty
            it "produces the TriTree [2(1()1 3()3)2]" $
                insert 1 (NodeOne 2 Empty (NodeOne 3 Empty Empty Empty) Empty) `shouldBe` NodeOne 2 (NodeOne 1 Empty Empty Empty) (NodeOne 3 Empty Empty Empty) Empty
        describe "insertList" $ do
            it "produces the TriTree [13(5-7(10)5-7) (NodeTwo 16 20 Empty Empty Empty) Empty)" $
                insertList [5,10,20] (NodeOne 13 (NodeOne 7 Empty Empty Empty) (NodeOne 16 Empty Empty Empty) Empty) `shouldBe` NodeOne 13 (NodeTwo 5 7 Empty Empty (NodeOne 10 Empty Empty Empty)) (NodeTwo 16 20 Empty Empty Empty) Empty
            it "produces the TriTree []" $
                insertList [] (Empty :: TriTree Int) `shouldBe` Empty
        describe "identical" $ do
            it "produces the boolean False" $
                identical Empty (NodeOne 3 Empty Empty Empty) `shouldBe` False
            it "produces the boolean True" $
                identical (NodeOne 3 Empty Empty Empty) (NodeOne 3 Empty Empty Empty) `shouldBe` True
            it "produces the boolean True" $
                identical (Empty :: TriTree Int) (Empty :: TriTree Int) `shouldBe` True
        describe "treeMap" $ do
            it "produces the TriTree []" $
                treeMap (\x -> x + 1) Empty `shouldBe` Empty
            it "produces the TriTree [3(2()2 4()4)]" $
                treeMap (\x -> x + 1) (NodeOne 2 (NodeOne 1 Empty Empty Empty) (NodeOne 3 Empty Empty Empty) Empty) `shouldBe` NodeOne 3 (NodeOne 2 Empty Empty Empty) (NodeOne 4 Empty Empty Empty) Empty
        describe "treeFoldPreOrder" $ do
            it "produces the integer 0" $
                treeFoldPreOrder (+) 0 (Empty :: TriTree Int) `shouldBe` 0
            it "produces the list [3,7,2,5,4,6,8]" $
                treeFoldPreOrder (++) [] (NodeTwo [3] [7] (NodeOne [2] Empty Empty Empty) (NodeOne [5] (NodeOne [4] Empty Empty Empty) (NodeOne [6] Empty Empty Empty) Empty) (NodeOne [8] Empty Empty Empty)) `shouldBe` [3,7,2,5,4,6,8]
        describe "treeFoldInOrder" $ do
            it "produces the integer 0" $
                treeFoldInOrder (+) 0 (Empty :: TriTree Int) `shouldBe` 0
            it "produces the list [2,3,4,5,6,7,8]" $
                treeFoldInOrder (++) [] (NodeTwo [3] [7] (NodeOne [2] Empty Empty Empty) (NodeOne [5] (NodeOne [4] Empty Empty Empty) (NodeOne [6] Empty Empty Empty) Empty) (NodeOne [8] Empty Empty Empty)) `shouldBe` [2,3,4,5,6,7,8]
        describe "treeFoldPostOrder" $ do
            it "produces the integer 0" $
                treeFoldPostOrder (+) 0 (Empty :: TriTree Int) `shouldBe` 0
            it "produces the list [2,4,6,5,8,3,7]" $
                treeFoldPostOrder (++) [] (NodeTwo [3] [7] (NodeOne [2] Empty Empty Empty) (NodeOne [5] (NodeOne [4] Empty Empty Empty) (NodeOne [6] Empty Empty Empty) Empty) (NodeOne [8] Empty Empty Empty)) `shouldBe` [2,4,6,5,8,3,7]
