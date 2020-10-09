import Data.List

main :: IO()
main = do
    print (twoChildrenNodes t1)
    print (allHaveTwoChildren t1)
    print (allHaveTwoChildren t2)
    print (biggestNumber [5, 6, 3, 2, 5, 1])
    print (intersectionPoints (\x -> x) (\x -> x * x) (-5) 5)
    print (intersectionPoints (\x -> x) (\x -> x * x + 1) (-5) 5)
    print (iterator [3, 4, 5] (+ 1))
    print (iterator [1, 2, 4] (+ 1))
    print (levelsum bt1 1)
    print (levelsum bt1 2)
    print (levelsum bt1 3)
    print (levelsum bt1 4)
    print (levelsum bt1 5)
    print (height bt1)
    print (cone bt1)
    print (cone bt2)
    print ((specialSum (5 -) [1..10]) (> 0))
    print ((specialSum1 (5 -) [1..10]) (> 0))
    print ((specialSum (\x -> x + 1) [(-5)..5]) odd)

t1 :: [(Int, [Int])]
t1 = [(1, [2]), (2, [3, 4]), (3, [5, 7]), (4, [1, 2, 3]), (5, [2, 3, 6]), (6, []), (7, [])]

t2 :: [(Int, [Int])]
t2 = [(1, [2, 4]), (2, [3, 4]), (3, [5, 7]), (4, [1, 2]), (5, [2, 3]), (6, []), (7, [])]

twoChildrenNodes :: [(Int, [Int])] -> [Int]
twoChildrenNodes ts = [v | (v, us) <- ts, length us == 2]

allHaveTwoChildren :: [(Int, [Int])] -> Bool
allHaveTwoChildren ts = length [v | (v, us) <- ts, length us > 0] == length (twoChildrenNodes ts)


-- Изпит, 27.06.2019 г.

biggestNumber :: [Int] -> Int
biggestNumber ns = foldr1 (\ n res -> res * 10 + n) (sort ns)

intersectionPoints :: (Int -> Int) -> (Int -> Int) -> Int -> Int -> [Int]
intersectionPoints f g a b = [k | k <- [a..b], f k == g k]

intersectionPoints1 :: (Int -> Int) -> (Int -> Int) -> Int -> Int -> [Int]
intersectionPoints1 f g a b = filter (\ x -> f x == g x) [a .. b]

iterator :: [Int] -> (Int -> Int) -> Bool
iterator []       _ = True
iterator [_]      _ = True
iterator (m:n:ks) f = f m == n && iterator (n:ks) f

data BTree = Empty | Node Int BTree BTree

bt1 :: BTree
bt1 = Node 1 (Node 2 (Node 4 Empty Empty)
                     (Node 5 Empty Empty))
             (Node 3 Empty
                     (Node 6 (Node 50 Empty Empty)
                             Empty))
        
bt2 = Node 10 (Node 2 (Node 4 Empty Empty)
                      (Node 5 Empty Empty))
              (Node 3 Empty
                      (Node 6 (Node 50 Empty Empty)
                              Empty))

levelsum :: BTree -> Int -> Int
levelsum Empty          _ = 0
levelsum (Node v _  _)  1 = v
levelsum (Node _ lt rt) k = levelsum lt (k - 1) + levelsum rt (k - 1)

height :: BTree -> Int
height Empty                = 0
height (Node _ Empty Empty) = 1
height (Node _ lt    rt)    = 1 + (max (height lt) (height rt)) 

cone :: BTree -> Bool
cone t = check [levelsum t k | k <- [1..height t]]
    where
        check []       = True
        check [_]      = True
        check (m:n:ks) = m < n && check (n:ks)


-- Контролно 2, 02.06.2019 г.
specialSum :: (Int -> Int) -> [Int] -> ((Int -> Bool) -> Int)
specialSum f xs = \ p -> sum [x * x | x <- xs, p (f x)]

specialSum1 :: (Int -> Int) -> [Int] -> ((Int -> Bool) -> Int)
specialSum1 f xs p = sum [x * x | x <- xs, p (f x)]