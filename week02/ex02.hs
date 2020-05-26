main :: IO()
main = do
    print(countDigits 123456) --01
    print(countDigitsIter 1234) --01 Iter
    print(sumDigits 12345) --02
    print(sumDigitsIter 12345) --02 Iter
    print(countOccurences 12221 1) --03
    print(isAscending 1234) --04.1
    print(isAscending' 4321) --04.2
    print(isPrime 124) --05


--01
countDigits :: Int -> Int
countDigits n
    | n < 10 = 1
    | otherwise = 1 + countDigits(n `div` 10)


--01 iterative
countDigitsIter :: Int -> Int
countDigitsIter n = countIter n 0
    where 
        countIter n digits 
            | n < 10 = digits + 1
            | otherwise = countIter (n `div` 10) (digits + 1)

--02
sumDigits :: Int -> Int
sumDigits n 
    | n < 10 = n
    | otherwise = (n `mod` 10) + sumDigits(n `div` 10)

--02 Iter
sumDigitsIter :: Int -> Int
sumDigitsIter n = sumIter n 0
    where
        sumIter n sum
            | n < 10 = sum + n
            | otherwise = sumIter (n `div` 10) (sum + (n`mod` 10))

--03
countOccurences :: Int -> Int -> Int
countOccurences n digit
    | n < 10 && n /= digit = 0
    | n < 10 && n == digit = 1
    | n `mod` 10  == digit = 1 + countOccurences (n `div` 10) digit
    | otherwise = countOccurences (n `div` 10) digit

--04.1
isAscending :: Integer -> Bool
isAscending n
    | n < 10 = True
    | otherwise = helper (n `div` 10) (n `mod` 10)
    where
        helper number lastDigit
            |number < 10 = number < lastDigit
            |(number `mod` 10) < lastDigit = helper (number `div` 10) (number `mod` 10)
            |otherwise = False

--04.2
isAscending' :: Integer -> Bool
isAscending' n
    | n < 10 = False
    | ((n `div` 10) `mod` 10) < (n `mod` 10) = isAscending' (n `div` 10)
    | otherwise = False

--05
isPrime :: Integer -> Bool
isPrime 1 = False
isPrime n = helper 2
    where
        helper current
            | current > n `div` 2 = True
            | (n `mod` current) == 0 = False
            | otherwise = helper (current + 1)