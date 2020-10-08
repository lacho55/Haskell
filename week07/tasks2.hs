-- Комбиниране/акумулиране на елементите на даден списък (fold-ване)
-- обхождане на списък с последователно прилагане на функция върху елементите му
foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ v []     = v
foldr' f v (x:xs) = f x (foldr' f v xs)
-- v е начална стойност. Например при foldr (+) най-вероятно ще е 0, при foldr (*) ще е 1

-- ако сме сигурни че ще работим с непразни списъци и искаме да не се налага отделно да подаваме начална стойност (а да започнем от стойностите в самия списък)
foldr1' :: (a -> a -> a) -> [a] -> a
foldr1' f [x]    = x
foldr1' f (x:xs) = f x (foldr1 f xs)

-- Примери за реализиране на други функции, ползвайки fold
concat' :: [[a]] -> [a]
concat' xs = foldr (++) [] xs

and' :: [Bool] -> Bool
and' bs = foldr (&&) True bs


-- Задача 0. Да се обърнат елементите на списък като се използва foldr. 
rev :: [a] -> [a]
rev lst = foldr (\x xs -> xs ++ [x]) [] lst           

rev' :: [a] -> [a]
rev' lst = foldr f [] lst
    where
        f :: a -> [a] -> [a]
        f x xs = xs ++ [x]

{- Как разсъждаваме за да решим задачата?
1. В общия случай имаме(например): (foldr f [] "abc")
2. Оценяваме:
(foldr f [] "abc")
('a' `f` (foldr f [] "bc"))
('a' `f` ('b' `f` (foldr f [] "c")))
('a' `f` ('b' `f` ('c' `f` (foldr f [] [])))) -- дъно
('a' `f` ('b' `f` ('c' `f` []))) -- първия аргумент на функцията е от същия тип като този на списъка, а втория аргумент е списък
('a' `f` ('b' `f` "c")) -- след изпълнение на функцията
('а' `f` "cb")          -- след изпълнение на функцията
-> "cba" 

3. конструираме f, така че да получим исканите междинни резултати, т.е
- ('c' `f` [])   -> "c"
- ('b' `f` "c")  -> "cb"
- ('а' `f` "cb") -> "cba"

4. Ясно е, че:
f :: a -> [a] -> [a]
f x xs = xs ++ [x]

5. Вместо да пишем (foldr f [] "abc"), може да напишем:
(foldr (\ x xs -> xs ++ [x]) [] "abc")  -}


-- Задача 1. Да се дефинира функция insert :: Int -> [Int] -> [Int], която добавя елемент в сортиран списък, като резултатният списък също е сортиран

-- Задача 2. Да се реализира функция insertionSort :: [Int] -> [Int], която реализира сортиране чрез вмъкване върху списък

-- Задача 3. Да се дефинира функция closestPoint ps, която приема списък ps от точки в равнината (представени чрез двойки (x, y)) 
-- и връща едноаргументна функция, чиято стойност в дадена точка p e най-близката до p точка от списъка ps.

main :: IO()
main = do
    --print (foldr (+) 54 [10, 11, 26, 7]) -- -> 108 
{- foldr - bracketing to the RIGHT
(foldr (+) 54 [10, 11, 26, 7]) ->
(10 + (foldr (+) 54 [11, 26, 7])) ->
(10 + (11 + (foldr (+) 54 [26, 7]))) ->
(10 + (11 + (26 + (foldr (+) 54 [7])))) ->
(10 + (11 + (26 + (7 + (foldr (+) 54 []))))) -> -- дъно
(10 + (11 + (26 + (7 + 54)))) ->
(10 + (11 + (26 + 61))) ->
(10 + (11 + 87)) ->
(10 + 98) ->
108 -}

    --print (foldl (+) 54 [10, 11, 26, 7]) -- -> 108
{- foldl - bracketing to the LEFT
(foldl (+) 54 [10, 11, 26, 7]) ->
(foldl (+) (54 + 10) [11, 26, 7]) -> 
(foldl (+) ((54 + 10) + 11) [26, 7]) -> 
(foldl (+) (((54 + 10) + 11) + 26) [7]) -> 
(foldl (+) ((((54 + 10) + 11) + 26) + 7) []]) -> -- дъно
((((54 + 10) + 11) + 26) + 7) ->
108 -}

-- обърнете внимание, че изваждането не е асоциативна операция
    --print (foldr (-) 54 [10, 11, 26, 7]) -- -> 72  = (10-(11-(26-(7-54)))
{- foldr - bracketing to the RIGHT
(foldr (-) 54 [10, 11, 26, 7]) ->
(10 - (foldr (-) 54 [11, 26, 7])) ->
(10 - (11 - (foldr (-) 54 [26, 7]))) ->
(10 - (11 - (26 - (foldr (-) 54 [7])))) ->
(10 - (11 - (26 - (7 - (foldr (-) 54 []))))) -> -- дъно
(10 - (11 - (26 - (7 - 54)))) ->
(10 - (11 - (26 - (-47)))) ->
(10 - (11 - 73)) ->
(10 - (-62)) ->
72 -}

    --print (foldl (-) 54 [10, 11, 26, 7]) -- -> 0   = ((((54-10)-11)-26)-7)
{- foldl - bracketing to the LEFT
(foldl (-) 54 [10, 11, 26, 7]) ->
(foldl (-) (54 - 10) [11, 26, 7]) -> 
(foldl (-) ((54 - 10) - 11) [26, 7]) -> 
(foldl (-) (((54 - 10) - 11) - 26) [7]) -> 
(foldl (-) ((((54 - 10) - 11) - 26) - 7) []]) -> -- дъно
((((54 - 10) - 11) - 26) - 7) ->
0 -}

    --print (foldr1 (-) [10, 11, 26, 7])   -- -> 18  = (10-(11-(26-7))
{- foldr - bracketing to the RIGHT
(foldr1 (-) [10, 11, 26, 7]) ->
(10 - (foldr1 (-) [11, 26, 7])) ->
(10 - (11 - (foldr1 (-) [26, 7]))) ->
(10 - (11 - (26 - (foldr1 (-) [7])))) -> -- дъно
(10 - (11 - (26 - 7))) ->
(10 - (11 - 19)) ->
(10 - (-8)) ->
18 -}

    --print (foldl1 (-) [10, 11, 26, 7])   -- -> -34 = (((10-11)-26)-7)
{- foldl - bracketing to the LEFT
(foldl1 (-) [10, 11, 26, 7]) -> -- !
(foldl (-) 10 [11, 26, 7]) ->   -- !
(foldl (-) (10 - 11) [26, 7]) -> 
(foldl (-) ((10 - 11) - 26) [7]) -> 
(foldl (-) (((10 - 11) - 26) - 7) []]) -> -- дъно
(((10 - 11) - 26) - 7) ->
-34 -}

    --print (foldr (\x y -> (x+y)/2) 54 [12, 4, 10, 6]) -- -> 12.0   = (12 o (4 o (10 o (6 o 54))), където x o y = (x+y)/2
    --print (foldl (\x y -> (x+y)/2) 54 [12, 4, 10, 6]) -- -> 10.125 = ((((54 o 12) o 4) o 10) o 6)
    --print (foldr (\x y -> concat ["(", x, "+", y, ")"]) "0" ["1", "2", "3"]) -- -> "(1+(2+(3+0)))"
    --print (foldl (\x y -> concat ["(", x, "+", y, ")"]) "0" ["1", "2", "3"]) -- -> "(((0+1)+2)+3)"

    --print (rev "abc")  -- -> "cba"
    --print (rev' "abc") -- -> "cba" 

    --print (insert 1 [2,3,4]) -- -> [1,2,3,4]
    --print (insert 3 [1,2,4]) -- -> [1,2,3,4]
    --print (insert 4 [1,2,3]) -- -> [1,2,3,4]
    --print (insertionSort [7,3,2,5,1,6,10,8,9,4]) -- -> [1,2,3,4,5,6,7,8,9,10]

    --print ((closestPoint [(0, 0), (1, 1), (2, 2)]) (0.9, 2)) -- -> (1.0, 1.0)
    --print ((closestPoint [(0, 0), (1, 1), (2, 2)]) (1, 2))   -- -> (2.0, 2.0) или (1.0, 1.0)?