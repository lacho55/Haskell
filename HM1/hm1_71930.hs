main::IO()
main = do
    --TASK01
    print(findSum 0 2 10)
    print(findSum 5 3 5)

    --TASK02
    print(isSquare 1)
    print(isSquare 2)
    print(isSquare 4)
    print(isSquare 17)
    print(isSquare 256)
    print(isSquare 2500)

    --TASK03
    print(isSpecial 131 2)
    print(isSpecial 472 2) 
    print(isSpecial 17197 2)
    print(isSpecial 12234 3) 
    print(isSpecial 10113 3) 
    print(isSpecial 353 2) 


--TASK01
findSum :: Int -> Int -> Int -> Int
findSum a b n --The main thing about this task is to use the previous calculations and add them to the new ones not just calculating everything again and again
    | n < 3 =  error ("The number must be more than 3")
    | otherwise = findSumHelper (a + b) 1 0
    where
        findSumHelper :: Int -> Int -> Int -> Int
        findSumHelper current i sum
            | i > n = sum
            | (i <= n - 3 ) = findSumHelper (current + (2^i * b)) (i + 1) 0
            | (i > n - 3) = findSumHelper (current + (2^i * b)) (i + 1) (sum + current)



--TASK02
isSquare :: Int -> Bool
isSquare x --Our main function which returns True and False when x is 0 or 1 and calls isSquareHelper in the other situations
    | x == 0 = True
    | x == 1 = True
    | x > 0 = isSquareHelper False 1
    where
        isSquareHelper :: Bool -> Int -> Bool -- A helper function that checks if a number is perfect  square without finding square root
        isSquareHelper state i
            | (i * i <= x) = isSquareHelper ((x `mod` i  == 0) && (x `div` i == i)) (i + 1) --It basically checks (i * i = n) 
            | otherwise = state


--TASK03
isPrime :: Int -> Bool
isPrime n = (isPrimeHelp n 2) --Checks if it is prime or not
  where 
    isPrimeHelp :: Int -> Int -> Bool --A standart function for checking wether a number is prime or not
    isPrimeHelp n k  --We use a helper function for that task
      | n == 1 = False 
      | n == k = True
      | (rem n k) == 0 = False --I have used 'rem' from the Haskell book it is almost equivalent to 'mod'. It has a difference when the second parameter is negative
      | otherwise = (isPrimeHelp n (k + 1))

len :: Int -> Int
len n = if (n < 1) then 0 else (1 + len (div n 10)) --Function len gives us the length of the number we are given

isSpecial :: Int -> Int -> Bool--And our main fuction that returns True when the length of our number is less than k and (True/False) depending on the other two functions
isSpecial n k = if ((len n) < k) then True else(isPrime (rem n (10^k))) &&  (isSpecial (div n 10) k) 