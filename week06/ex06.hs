main :: IO()
main = do
    print (any' odd [1,2,3]) -- -> True
    print (any' odd [2,4,6]) -- -> False
    print (all' odd [1,2,3]) -- -> False
    print (all' odd [1,3,5]) -- -> True
    print (map' (\x -> x*x) [1,2,3]) -- -> [1,4,9]
    print (filter' odd [1,2,3,4])    -- -> [1,3]
    print (zipWith' (+) [1,2,3] [4,5,6]) -- -> [5,7,9]
    print (incrementAllBy [1,2,3] 3)    -- -> [4,5,6]
    print (multiplyAllBy [1,2,3] 3)     -- -> [3,6,9]
    print (filterSmallerThan [1,2,3] 3) -- -> [1,2]
    print (splitByParity [1, 2, 3, 4])     -- -> ([1,3], [2,4])
    print (partition' (<5) [0..10])        -- -> ([0,1,2,3,4], [5,6,7,8,9,10])
    print (quickSort [3,2,5,1,6,10,8,9,4]) -- -> [1,2,3,4,5,6,8,9,10]





any' :: (a -> Bool) -> [a] -> Bool
any' _ [] = False
any' p (x:xs) = p x || any' p xs


all' :: (a -> Bool) -> [a] -> Bool
all' _ [] = True
all' p (x:xs) = p x && all' p xs


map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' p (x:xs) = p x : map' p xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs) = if (p x) then x : filter' p xs else filter' p xs

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (a:as) (b:bs) = f a b : zipWith' f as bs

isAscending :: Integer -> Bool
isAscending num = all ordered (zip digits (tail digits))
    where
        numToList x = if (x < 10) then [x] else numToList (x `div` 10) ++ [x `mod` 10]
        digits = numToList num
        ordered (x,y) = x <= y


incrementAllBy :: [Int] -> Int -> [Int]
--incrementAllBy xs n = map (\ x -> x + n) xs -- 1ви вариант, ползвайки ламбда функция
incrementAllBy xs n = map (+n) xs  


multiplyAllBy :: [Int] -> Int -> [Int]
multiplyAllBy xs n = map (*n) xs


filterSmallerThan :: [Int] -> Int -> [Int]
filterSmallerThan xs n = filter (<n) xs

splitByParity :: [Int] -> ([Int], [Int])
splitByParity xs = (filter odd xs, filter even xs)


partition' :: (a -> Bool) -> [a] -> ([a], [a])
partition' p xs = (filter p xs, filter (not . p) xs)

splitByParity' :: [Int] -> ([Int], [Int])
splitByParity' xs = partition' odd xs

quickSort :: [Int] -> [Int]
quickSort []     = []
quickSort (p:xs) = quickSort smaller ++ [p] ++ quickSort larger
    where (smaller, larger) = partition' (<p) xs