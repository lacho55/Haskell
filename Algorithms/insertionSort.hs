main :: IO()
main = do
    print(iSort [3,1,2])


iSort :: [Int] -> [Int]
iSort [] = []
iSort (x:xs) = ins x (iSort xs)
    where
        ins :: Int -> [Int] -> [Int]
        ins x [] = [x]
        ins x (y:ys)
            | x <= y = x : (y: ys)
            | otherwise = y : ins x ys