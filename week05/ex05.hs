main :: IO()
main = do
    print (removeFirst 2 [1,2,3,2,4]) -- -> [1,3,2,4]
    print (removeFirst 5 [1,2,3,2,4]) -- -> [1,2,3,2,4]
    print (removeAll 2 [1,2,3,2,4])   -- -> [1,3,4]





removeFirst :: Eq a => a -> [a] -> [a]
removeFirst _ [] = []
removeFirst x (y:ys) = if(x == y) then ys else y : removeFirst x ys

removeAll :: Eq a => a -> [a] -> [a]
removeAll _ [] = []
removeAll x (y:ys) = if (x == y) then removeAll x ys else y : removeAll x ys
