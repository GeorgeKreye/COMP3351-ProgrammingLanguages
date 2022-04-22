module TriTree where
    data TriTree a = Empty | 
                 NodeOne a (TriTree a) (TriTree a) (TriTree a) | 
                 NodeTwo a a (TriTree a) (TriTree a) (TriTree a) 
                 deriving (Show)
