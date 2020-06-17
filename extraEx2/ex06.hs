main :: IO()
main = do
    print $ maxDepthBlueNode colorTree        -- -> 3



data Color = Red | Green | Blue deriving (Read, Show, Eq)

data BTree a = Empty | Node a (BTree a) (BTree a) deriving Show

colorTree :: BTree Color                                            --        Blue
colorTree = Node Blue (Node Red (Node Green Empty Empty)            --       /    \
                                Empty)                              --    Red      Red
                      (Node Red (Node Blue (Node Green Empty Empty) --    /        /
                                           (Node Red Empty Empty))  -- Green     Blue
                                Empty)                              --           /   \
                                                                    --        Green  Red

maxDepthBlueNode :: BTree Color -> Int
maxDepthBlueNode tree = func tree 1
    where 
        func Empty _ = 0
        func (Node Blue left right) curDepth = maximum [curDepth, func left (curDepth + 1), func right (curDepth + 1)]
        func (Node _ left right) curDepth = max (func left (curDepth + 1)) (func right (curDepth + 1))