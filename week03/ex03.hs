main :: IO()
main = do
    print (isPerfect 6)   -- -> True
    print (isPerfect 18)  -- -> False
  
    print (reverseNumber 5)   -- -> 5
    print (reverseNumber 123) -- -> 321
    print (reverseNumber 95)  -- -> 59

    print (isPalindrome 12321)     -- -> True
    print (isPalindrome 1232)      -- -> False
    print (isPalindrome 1)         -- -> True
    print (isPalindrome 123454321) -- -> True


    -- Упражнение 3: n-торки
    print a       -- -> (1,2)
    print b       -- -> (1,2.3,4.5)

    -- може да достъпваме първи и втори елемент с fst и snd съответно (само при двойки)
    print (fst a) -- -> 1
    print (snd a) -- -> 2
    print (fst b) -- Couldn't match expected type `(a0, b0)' with actual type `(Int, Float, Float)'
 
    -- може да ги сравняваме с ==, но само ако са с еднаква дължина и типове
    print ((1,2) == (1,2)) -- -> True
    print ((1,2) == (1,1)) -- -> False
    print ((1,2,3) == (1,2)) -- error: Couldn't match expected type `(Integer, Integer, Integer)' with actual type `(Integer, Integer)'
 
    print grade1 -- -> ("John","Algebra",4.75)
    print grade2 -- -> ("Jane","Geometry",5.25)
    
 
    print (addPair (3,4)) -- -> 7
    print (addPair' (3,4))
 
    print (divide 7 2) -- -> (3,1)

    print (sumVectors   (1, 1, 1) (2, 3, 5)) -- -> (3.0,4.0,6.0)
    print (scaleVector  (2, 3, 5) 3)         -- -> (6.0,9.0,15.0)
    print (dotProduct   (1, 1, 1) (2, 3, 5)) -- -> 10.0
    print (crossProduct (1, 1, 1) (2, 3, 5)) -- -> (2.0,-3.0,1.0)
    print (magnitude    (1, 1, 1))           -- -> 1.7320508075688772

    print (sumRat (1,2) (3,2))      -- -> (8,4)
    print (sumRat' (1,2) (3,2))     -- -> (8,4)
    print (multiplyRat (1,2) (4,5)) -- -> (4,10)
    print (divideRat (1,2) (4,5))   -- -> (5,8)
    print (normalizeRat (500,1025)) -- -> (20,41)
    print (sumRatNorm (1,2) (3,2))  -- -> (2,1)




--01
isPerfect :: Integer -> Bool
isPerfect n = n == (sumDivisors 1 2)
    where
        sumDivisors sum current
            | current > n `div` 2 = sum
            | n `mod` current == 0 = sumDivisors (sum + current) (current + 1)
            | otherwise = sumDivisors sum (current + 1)

--02
reverseNumber :: Integer -> Integer
reverseNumber n = helper n 0
    where
        helper rest acc
            | rest < 10 = rest + acc * 10
            | otherwise = helper (rest `div` 10) ((rest `mod` 10) + acc * 10)

--03
isPalindrome :: Integer -> Bool
isPalindrome n = n == reverseNumber n

--Упражнение 03

a :: (Int, Int)
a = (1,2)

b :: (Int, Float, Float)
b = (1, 2.3, 4.5)

type Grade = (String, String, Float)
grade1 :: Grade
grade2 :: Grade
grade1 = ("John", "Algebra", 6)
grade2 = ("Jane", "Geometry", 2)


--01
addPair :: (Int, Int) -> Int
addPair (x, y) = x + y

addPair' :: (Int, Int) -> Int
addPair' p = fst p + snd p 

addTripple :: (Int, Int, Int) -> Int
addTripple (x, _, z) = x + z

--02
divide :: Int -> Int -> (Int, Int)
divide x y = (x `div` y, x `mod` y)

--03
--a)
type Vector = (Double, Double, Double)
sumVectors :: Vector -> Vector -> Vector
sumVectors (x1, y1, z1) (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)

--b)
scaleVector :: Vector -> Double -> Vector
scaleVector (x, y, z) p = (x * p, y * p, z * p)

--c)
dotProduct :: Vector -> Vector -> Double
dotProduct (x1, y1, z1) (x2, y2, z2) = x1 * x2 + y1 * y2 + z1 * z2

crossProduct :: Vector -> Vector -> Vector
crossProduct (x1, y1, z1) (x2, y2, z2) = (y1 * z2 - z1 * y2, z1 * x2 - x1 * z2, x1 * y2 - y1 * x2)

-- e) magnitude :: Vector -> Double, която намира дължината на даден вектор
magnitude :: Vector -> Double
magnitude (x, y, z) = sqrt (x * x + y * y + z * z)

-- Задача 4. Да се дефинира тип Rat, представящ рационално число като двойка от цели числа - числител и знаменател
type Rat = (Int, Int)
-- Да се дефинират функции за работа с рационални числа:
-- a) sumRat, която сумира две рационални числа
-- първи вариант - с fst и snd
sumRat :: Rat -> Rat -> Rat
sumRat x y = (fst x * snd y + fst y * snd x, snd x * snd y)

-- втори вариант - с pattern matching
sumRat' :: Rat -> Rat -> Rat
sumRat' (a, b) (c, d) = (a * d + b * c, b * d)

-- b) multiplyRat, която умножава две рационални числа
multiplyRat :: Rat -> Rat -> Rat
multiplyRat (a, b) (c, d) = (a* c, b * d)

-- c) divideRat, която разделя рационално число на друго рационално число
divideRat :: Rat -> Rat -> Rat
divideRat (a, b) (c, d) = multiplyRat (a,b) (d,c)

-- d) equalRat, която проверява дали две рационални числа са равни
-- Забележка: две рационални числа с различни числител и знаменател може да са равни, например 1/2 == 2/4 == 4/8 == ...
equalRat :: Rat -> Rat -> Bool
equalRat (a, b) (c, d) = a * d == b * c

-- e) normalizeRat x, която трансформира рационалното число x в равно на него рационално число, такова че числителят и знаменателят имат НОД 1
normalizeRat :: Rat -> Rat
normalizeRat (a, b) = (a `div` d, b `div` d)
    where d = gcd a b 

-- f) да се пренапише sumRat, така че резултата винаги да е "нормализирано" рационално число, тоест такова, което изпълнява условията от е)
sumRatNorm :: Rat -> Rat -> Rat
sumRatNorm x y = normalizeRat (sumRat x y)

