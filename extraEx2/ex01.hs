main::IO()
main = do
    print(1)




func :: (Int -> Int) -> [Int] -> (Int -> Int)
func f lst = resultFunc
    where resultFunc x = sum [n * f(x * a) | a <- lst, n <- [1..length lst]]

func2 :: (Int -> Int) -> [Int] -> (Int -> Int)
func2 f lst = resultFunc
    where resultFunc x = sum [a * f (x ^ n) | a <- lst, n <- [1.. length lst]]
    

boundUp :: (Int -> Int) -> Int -> (Int -> Int)
boundUp f y = resultFunc
    where resultFunc x = if (f(x) > y) then f(x) else f(y)


compositionOfOdds :: [(Int -> Int)] -> (Int -> Int)
compositionOfOdds [] = id
compositionOfOdds (f:fs) = f.compositionOfOdds fs

compareFuncsWithOddComp :: [(Int -> Int)] -> (Int -> Int) -> (Int -> Int)
compareFuncsWithOddComp [] oddFuncsComp = id
compareFuncsWithOddComp (f:fs) oddFuncsComp = resultFunc
    where resultFunc x = if(f(x) == oddFuncsComp(x)) then f(x) else compareFuncsWithOddComp fs oddFuncsComp x

getOddCompositionValue :: [(Int -> Int)] -> (Int -> Int) -> (Int -> Int)
getOddCompositionValue lst = compareFuncsWithOddComp lst (compositionOfOdds oddFuncs)
    where
        oddFuncs = [a | a <- lst, n <- [1.. length lst], n `mod` 2 == 1]