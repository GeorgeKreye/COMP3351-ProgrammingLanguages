{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldl" #-}
module TriTree where
    -- Hopefully error(s) isn't/aren't in here or that's a fundamental issue
    data TriTree a = Empty |
        NodeOne a (TriTree a) (TriTree a) (TriTree a) |
        NodeTwo a a (TriTree a) (TriTree a) (TriTree a)
        deriving (Show, Eq)
    {-
        Given a value and a TriTree, returns a boolean of whether
        the TriTree contains the value
    -}
    search :: Ord a => a -> TriTree a -> Bool
    search _ Empty = False
    search v (NodeOne x l m r)=
        (x == v) || (search v l || search v m || search v r)
    search v (NodeTwo x y l m r) =
        (x == v || y == v) || (search v l || search v m || search v r)
    {- 
        Given a value and a TriTree, inserts the value as new NodeOne leaf in the proper order
    -}
    insert :: Ord a => a -> TriTree a -> TriTree a
    insert v Empty = NodeOne v Empty Empty Empty
    insert v (NodeOne x l m r)
      | l == Empty && m == Empty && r == Empty =
            if v > x
                then NodeTwo x v Empty Empty Empty
                else NodeTwo v x Empty Empty Empty
      | v < x = NodeOne x (insert v l) m r
      | otherwise = NodeOne x l (insert v m) r
    insert v (NodeTwo x y l m r)
      | v < x = NodeTwo x y (insert v l) m r
      | v >= x && v < y = NodeTwo x y l (insert v m) r
      | otherwise = NodeTwo x y l m (insert v r)
    {- 
        Given a list of values and a TriTree, inserts the values as new NodeOnes in the proper order
    -}
    insertList :: Ord a => [a] -> TriTree a -> TriTree a
    insertList [] t = t
    insertList (x:xs) t = insertList xs (insert x t)
    {- 
        Given two TriTrees, returns True if they are identical and False if not
    -}
    identical :: Eq a => TriTree a -> TriTree a -> Bool
    identical Empty Empty = True
    identical NodeOne {} Empty = False
    identical Empty NodeOne {} = False
    identical NodeTwo {} Empty = False
    identical Empty NodeTwo {} = False
    identical NodeOne {} NodeTwo {} = False
    identical NodeTwo {} NodeOne {} = False
    identical (NodeOne x a b c) (NodeOne y d e f) = x == y && identical a d && identical b e && identical c f
    identical (NodeTwo x y a b c) (NodeTwo w z d e f) = x == w && y == z && identical a d && identical b e && identical c f
    {- 
        Given a function and a TriTree, maps that function to create a new TriTree
    -}
    treeMap :: (a -> b) -> TriTree a -> TriTree b
    treeMap _ Empty = Empty
    treeMap (f :: a -> b) (NodeOne v l m r) = NodeOne (f v) (treeMap f l) (treeMap f m) (treeMap f r)
    treeMap (f :: a -> b) (NodeTwo x y l m r) = NodeTwo (f x) (f y) (treeMap f l) (treeMap f m) (treeMap f r)
    {- 
        Given a function, an initial value, and a TriTree, combines all values of the TriTree using the function in preorder (root value(s) before subtrees)
    -}
    treeFoldPreOrder :: (a -> a -> a) -> a -> TriTree a -> a
    treeFoldPreOrder _ v Empty = v
    treeFoldPreOrder f v (NodeOne x l m _) =
        let y = f v x
            z = treeFoldPreOrder f y l
        in
            treeFoldPreOrder f z m
    treeFoldPreOrder f v (NodeTwo x y l m r) =
        let a = f v x
            b = f a y
            c = treeFoldPreOrder f b l
            d = treeFoldPreOrder f c m
        in
            treeFoldPreOrder f d r
    {- 
        Given a function, an initial value, and a TriTree, combines all values of the TriTree using the function in order (left subtree, lesser root value,
        middle subtree, etc.)
    -}
    treeFoldInOrder :: (a -> a -> a) -> a -> TriTree a -> a
    treeFoldInOrder _ v Empty = v
    treeFoldInOrder f v (NodeOne x l m _) =
        let y = treeFoldInOrder f v l
            z = f y x
        in
            treeFoldInOrder f z m
    treeFoldInOrder f v (NodeTwo x y l m r) =
        let a = treeFoldInOrder f v l
            b = f a x
            c = treeFoldInOrder f b m
            d = f c y
        in
            treeFoldInOrder f d r
    {- 
        Given a function, an initial value, and a TriTree, combines all values of the TriTree using the function in postorder (subtrees before root
        value(s))
    -}
    treeFoldPostOrder :: (a -> a -> a) -> a -> TriTree a -> a
    treeFoldPostOrder _ v Empty = v
    treeFoldPostOrder f v (NodeOne x l m _) =
        let y = treeFoldPostOrder f v l
            z = treeFoldPostOrder f y m
        in
            f z x
    treeFoldPostOrder f v (NodeTwo x y l m r) =
        let a = treeFoldPostOrder f v l
            b = treeFoldPostOrder f a m
            c = treeFoldPostOrder f b r
            d = f c x
        in
            f d y

    {- 
        CREDIT
        types and Empty cases for treeFold* functions taken and modified from https://stackoverflow.com/questions/39180630/fold-tree-function
    -}
