module Main where
    
import Tree

main :: IO ()

tree =  let l = Leaf()
            n = Node
        in n (n (n l l) l) (n l l)

main = print $ show (labelTree tree)
