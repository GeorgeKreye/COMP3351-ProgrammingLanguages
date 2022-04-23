module TriTree where
    data TriTree a = Empty |
        NodeOne a (TriTree a) (TriTree a) (TriTree a) |
        NodeTwo a a (TriTree a) (TriTree a) (TriTree a)
        deriving (Show)
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
        TODO: Fix error
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
