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
    insert v (NodeOne x l m r) =
        if v < x
            then NodeOne x (insert v l) m r
            else NodeOne x l (insert v m) r
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
    treeFoldPreOrder :: (a -> b -> b) -> b -> TriTree a -> b
    treeFoldPreOrder _ v Empty = v
    {- 
        Given a function, an initial value, and a TriTree, combines all values of the TriTree using the function in order (left subtree, lesser root value,
        middle subtree, etc.)
    -}
    treeFoldInOrder :: (a -> b -> b) -> b -> TriTree a -> b
    treeFoldInOrder _ v Empty = v
    {- 
        Given a function, an initial value, and a TriTree, combines all values of the TriTree using the function in postorder (subtrees before root
        value(s))
    -}
    treeFoldPostOrder:: (a -> b -> b) -> b -> TriTree a -> b
    treeFoldPostOrder _ v Empty = v

    {- 
        CREDIT
        type definitions for treeFold* functions taken and modified from https://stackoverflow.com/questions/39180630/fold-tree-function
    -}