module Tree
    ( labelTree
    ) where

import Stream
import Supply

-- use supply monad to label the leaves of a tree from left to right with natural numbers
data Tree a = Node (Tree a) (Tree a) | Leaf a deriving Show

labelTree :: Tree a -> Tree Integer
labelTree t = runSupply nats (go t)
    where
        go :: Tree a -> Supply s (Tree s)
        go (Node fstNode sndNode) = Node <$> go fstNode <*> go sndNode
        go (Leaf x) = Leaf <$> get
