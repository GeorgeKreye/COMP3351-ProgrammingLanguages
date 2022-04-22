module TriTree where
    data TriTree a = Empty |
                 NodeOne a (TriTree a) (TriTree a) (TriTree a) |
                 NodeTwo a a (TriTree a) (TriTree a) (TriTree a)
                 deriving (Show)
    {-
        Given a value and a TriTree, returns a boolean of whether
        the TriTree contains the value
    -}
    search :: Ord v => v -> TriTree t -> Bool
    search v t = False;
