import Data.List(tails)
import Data.Char(isLetter, isDigit)

-- Задача 1. Да се дефинира предикат checkSequence :: [Int] -> Bool, който приема списък от цели числа [a1, a2, …, an] и проверява дали следните две условия са изпълнени:
-- 1. за всяко i < j: ai < aj,
-- 2. за всяко i < j: aj `mod` ai ≠ 0.
checkSequence :: [Int] -> Bool
checkSequence [] = True
checkSequence xs = isSorted xs && all condition (tails xs)
    where 
        isSorted xs = and $ zipWith (<) xs (tail xs)

        condition []     = True
        condition (x:xs) = all (\ y -> y `mod` x /= 0 ) xs


-- Задача 2. Да се дефинира функция removeNb :: Int -> [(Int, Int)], която приема естествено число n и връща списък от двойки естествени числа (a, b) – такива, че:
-- 1. a и b са по-малки от n,
-- 2. тяхното произведение е равно на сумата от числата от 1 до n без a и b.
removeNb :: Int -> [(Int, Int)]
removeNb n = [(a, b) | a <- [1..n], b <- [1..n], a * b == sum [x | x <- [1..n], x /= a, x /= b]]

-- Задача 3. Ако A(x1,y1) и B(x2,y2) са две точки в декартовата равнина – такива, че x1 ≠ x2, то уравнението на правата AB, 
-- която минава през тези две точки, е y = f(x), където f(x) = y1 + (x-x1)*(y2-y1)/(x2-x1).
-- а) Нека имаме тип точка, представен така:
type Point = (Double, Double)
-- Да се дефинира функция line :: Point -> Point -> (Double -> Double), която по две точки връща функцията, определяща уравнението на минаващата през тях права.
line :: Point -> Point -> (Double -> Double)
line (x1, y1) (x2, y2) = \ x -> y1 + (x - x1) * (y2 - y1) / (x2 - x1)
-- б) Напишете функция от по-висок ред liesOn :: (Double -> Double) -> (Point -> Bool), която за дадена функция f, определяща уравнението на права, 
-- връща като резултат функция, която по дадена точка P(x,y) проверява дали точката P лежи на правата f (дали y = f(x)).
liesOn :: (Double -> Double) -> (Point -> Bool)
liesOn f = \ (x, y) -> y == f x -- abs (y - (f x)) < 1e-6 because of floating point errors

diagonal = line (0,0) (1,1)
onDiag = liesOn diagonal


main :: IO()
main = do
    --print $ checkSequence [2, 9, 15]           -- -> True
    --print $ checkSequence [11, 14, 20, 27, 31] -- -> True
    --print $ checkSequence [11, 14, 28, 27, 31] -- -> False
    --print $ checkSequence [11, 14, 14, 29, 31] -- -> False

    --print $ removeNb 10  -- -> [(6,7),(7,6)]
    --print $ removeNb 11  -- -> []
    --print $ removeNb 17  -- -> [(10,13),(13,10)]
    --print $ removeNb 20  -- -> [(14,14)]
    --print $ removeNb 26  -- -> [(15,21),(21,15)]
    --print $ removeNb 100 -- -> []
    --print $ removeNb 101 -- -> [(55,91),(91,55)]

    --print $ diagonal 5.5 -- -> 5.5
    --print $ diagonal 0.5 -- -> 0.5
    --print $ onDiag (5.5, 5.5) -- -> True
    --print $ onDiag (0.5, 0)   -- -> False