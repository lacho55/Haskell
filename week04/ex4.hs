main::IO()
main  = do
    print (append [1,2,3] [4,5,6])
    print (reverse' [1,2,3])
    print (concat' [[1],[2]])
    print (zip' [1,2] [3,4])
    print (sublistBetween 1 2 [1,2,3,4,5])
    print (chunksOf 1 [1,2,3])
    print (isSorted [1,2,3])
    print (isAscending 123)
    print (merge [1, 3, 5] [2, 4, 6]) -- -> [1, 2, 3, 4, 5, 6]
    print (isPrime 17) -- -> True
    print (isPrime 14) -- -> False
    print (isPerfect 6)  -- -> True
    print (isPerfect 18) -- -> False
    print (primesInRange 1 10)    -- -> [2, 3, 5, 7]
    print (primesInRange 1 2)     -- -> [2]
    print (perfectsInRange 1 100) -- -> [6, 28]
    print (prodSumDiv [1..10] 3) -- -> 4800
    print (squares 1 4 0.5) -- -> [(1.0,1.0),(1.5,2.25),(2.0,4.0),(2.5,6.25),(3.0,9.0),(3.5,12.25),(4.0,16.0)]
    print (getVolume [(5, 4), (1, 1), (2.3, 9.8)]) -- -> [314.1592653589793,3.141592653589793,162.86644634740205]   
    print (getVolume' [(5, 4), (1, 1), (2.3, 9.8)])  -- -> [((5.0,4.0),314.1592653589793),((1.0,1.0),3.141592653589793),((2.3,9.8),162.86644634740205)]



append :: [a] -> [a] -> [a]
append [] ys = ys
append xs [] = xs
append (x:xs) ys = x : (append xs ys)

reverse' :: [a] -> [a]
reverse' xs = helper [] xs
    where 
        helper :: [a] -> [a] -> [a]
        helper acc []   = acc
        helper acc (y:ys) = helper (y : acc) ys


concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys


insertAt :: Int -> a -> [a] -> [a]
insertAt 0 x [] = [x]
insertAt _ _ [] = error "No such position"
insertAt 0 x xs = x : xs
insertAt n x (y:ys) = y : insertAt (n - 1) x ys


sublistBetween :: Int -> Int -> [a] -> [a]
sublistBetween start end xs = take (end - start) (drop start xs)

chunksOf :: Int -> [a] -> [[a]]
chunksOf size xs = if length xs <= size then [xs] else take size xs : chunksOf size (drop size xs)

isSorted :: [Int] -> Bool
isSorted [] = True
isSorted (x:[]) = True
isSorted (x1:x2:xs) = x1 <= x2 && isSorted (x2:xs)

isAscending :: Int -> Bool
isAscending num = isSorted (numToList num)
    where
        numToList x = if(x < 10) then [x] else numToList (x `div` 10) ++ [x `mod` 10]


merge :: [Int] -> [Int] -> [Int]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | x <= y = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

isPrime :: Integer -> Bool
isPrime 1 = False
isPrime n = (length [x | x <- [2..n-1], n `mod` x == 0]) == 0

isPerfect :: Integer -> Bool
isPerfect n = n == sum [x | x <- [1..n-1], n `mod` x == 0]

primesInRange :: Integer -> Integer -> [Integer]
primesInRange a b 
    | a > b = []
    | isPrime a = a : (primesInRange (a + 1) b)
    | otherwise = primesInRange (a + 1) b 


primesInRange' :: Integer -> Integer -> [Integer]
primesInRange' a b = [x | x <- [a..b], isPrime x]

perfectsInRange :: Integer -> Integer -> [Integer]
perfectsInRange a b = [x | x <- [a..b], isPerfect x]


prodSumDiv :: [Integer] -> Integer -> Integer
prodSumDiv xs k = product [x | x <- xs, (divisorSum x) `mod` k == 0]
    where
        divisorSum n = sum [divisor | divisor <- [1..n], n `mod` divisor == 0]

squares :: Double -> Double -> Double ->[(Double, Double)]
squares a b h = [(x, x * x) | x <- [a, a + h .. b]]

type Cylinder = (Double, Double)
getVolume :: [Cylinder] -> [Double]
getVolume cylinders = [pi * r * r * h | (r, h) <- cylinders]


getVolume' :: [Cylinder] -> [(Cylinder, Double)]
getVolume' cylinders = [(c, pi*r*r*h) | c@(r, h) <- cylinders]