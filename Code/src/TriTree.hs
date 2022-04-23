module TriTree where
    data TriTree a = Empty |
        NodeOne a (TriTree a) (TriTree a) (TriTree a) |
        NodeTwo a a (TriTree a) (TriTree a) (TriTree a)
        deriving (Show)
    {-
        Given a value and a TriTree, returns a boolean of whether
        the TriTree contains the value
        TODO: implement
    -}
    search :: Ord a => a -> TriTree a -> Bool
    search _ Empty = False
    search v (NodeOne x l m r)=
        (x == v) || (search v l || search v m || search v r)
    search v (NodeTwo x y l m r) =
        (x == v || y == v) || (search v l || search v m || search v r)
