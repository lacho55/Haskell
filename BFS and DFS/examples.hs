main :: IO()
main = do



data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

traverseDF :: Tree a -> [a]
traverseDF Empty        = []
traverseDF (Node a l r) = a : (traverseDF l) ++ (traverseDF r)


traverseBF :: Tree a -> [a]
traverseBF tree = tbf [tree]
    where
        tbf [] = []
        tbf xs = map nodeValue xs ++ tbf (concat (map leftAndRightNodes xs))
        nodeValue (Node a _ _) = a
        leftAndRightNodes (Node _ Empty Empty) = []
        leftAndRightNodes (Node _ Empty b)     = [b]
        leftAndRightNodes (Node _ a Empty)     = [a]
        leftAndRightNodes (Node _ a b)         = [a,b]


createTree = Node 'A'
                (Node 'B'
                    (Node 'C' Empty Empty)
                    (Node 'D' Empty Empty)
                )
                (Node 'E'
                    (Node 'F' Empty Empty)
                    (Node 'G' Empty (Node 'H'
                        (Node 'I' Empty Empty)
                        Empty
                    ))
                )