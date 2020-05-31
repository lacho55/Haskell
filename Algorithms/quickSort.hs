main :: IO()
main = do
    print(qSort [2,3,1])


qSort :: [Int] -> [Int]
qSort [] = []
qSort (x: xs) = [y | y <- xs, y <= x] ++ [x] ++ qSort [y | y <- xs, y > x]