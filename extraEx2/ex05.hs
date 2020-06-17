main :: IO()
main = do
        print $ maxDepthGreenNode colorTree        -- -> 3



data Color = Red | Green | Blue deriving (Read, Show, Eq)

data BTree a = Empty | Node a (BTree a) (BTree a) deriving Show

colorTree :: BTree Color                                            --        Blue
colorTree = Node Blue (Node Red (Node Green Empty Empty)            --       /    \
                                Empty)                              --    Red      Red
                      (Node Red (Node Blue (Node Green Empty Empty) --    /        /
                                           (Node Red Empty Empty))  -- Green     Blue
                                Empty)                              --           /   \
                                                                    --        Green  Red

maxDepthGreenNode :: BTree Color -> Int
maxDepthGreenNode tree = func tree 1
    where 
        func Empty _ = 0
        func (Node Green left right) curDepth = minimum [curDepth, func left (curDepth + 1), func right (curDepth + 1)]
        func (Node _ left right) curDepth = min (func left (curDepth + 1)) (func right (curDepth + 1))