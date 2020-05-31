main :: IO()
main = do
    print (removeFirst 2 [1,2,3,2,4]) -- -> [1,3,2,4]
    print (removeFirst 5 [1,2,3,2,4]) -- -> [1,2,3,2,4]
    print (removeAll 2 [1,2,3,2,4])   -- -> [1,3,4]
    print (removeDuplicates [1,2,1,4,3,4,5,5,5,4,5]) -- -> [1,2,4,3,5]
    print (prefix [1,2,3] [1,2,3,4,5]) -- -> True
    print (prefix [1,2,4] [1,2,3,4,5]) -- -> Fals
    print (inits [1,2,3]) -- -> [[],[1],[1,2],[1,2,3]]
    print (tails [1,2,3]) -- -> [[1,2,3],[2,3],[3],[]]
    print (prefix' [1,2,3] [1,2,3,4,5]) -- -> True
    print (prefix' [1,2,4] [1,2,3,4,5]) -- -> False
    print (countOccurences [1] [1,1,1,1]) -- -> 4
    print (countOccurences [1,2,3] [1,2,3,4,2,3,1,2,3,4,1,2]) -- -> 2





removeFirst :: Eq a => a -> [a] -> [a]
removeFirst _ [] = []
removeFirst x (y:ys) = if(x == y) then ys else y : removeFirst x ys


removeAll :: Eq a => a -> [a] -> [a]
removeAll _ [] = []
removeAll x (y:ys) = if (x == y) then removeAll x ys else y : removeAll x ys


removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs) = x : removeDuplicates (removeAll x xs)


prefix :: Eq a => [a] -> [a] -> Bool
prefix xs ys = xs == take (length xs) ys

countOccurences :: Eq a => [a] -> [a] -> Int
countOccurences subxs xs = helper xs 0
    where
        helper []            cnt = cnt
        helper curr@(_:rest) cnt = if prefix subxs curr then helper rest (cnt + 1) else helper rest cnt 