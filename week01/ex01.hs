main::IO()
main = do
    print (inside 2 1 5)--01
    print (sumSquares 2 2) --02
    print (average 2 5) --03
    print (squaresAverage 4 5) --04
    print (myMin 8 9) --05.1
    print (myMin' 8 9) --05.2
    print (myFact 5) --06
    print (myFactIter 5) --07
    print (myFib 3) --08


--01
inside :: Int -> Int -> Int-> Bool
inside x a b = (x >= a) && (x <= b)

--02
sumSquares :: Int -> Int -> Int
sumSquares a b = a^2 + b^2

--03
average :: Int -> Int -> Double
average a b = fromIntegral (a + b) / 2

--04
squaresAverage :: Int -> Int -> Double
squaresAverage x y = average (x * x) (y * y)

--05.1
myMin :: Int -> Int -> Int
myMin a b = if(a >= b) then a else b

--05.2
myMin' :: Int -> Int -> Int
myMin' a b 
    | a >= b = a
    | otherwise = b


--06
myFact :: Int -> Int
myFact n
    | n == 0 = 1
    | n < 0  = error "Invalid argument!"
    | otherwise = n * myFact(n - 1)


--07
myFactIter :: Int -> Int
myFactIter n = helper n 1
    where 
        helper 0 x = x
        helper n x = helper (n - 1) (x * n)

--08
myFib :: Int -> Int
myFib n
    | n == 0 = 0
    | n == 1 = 1
    | otherwise = myFib(n - 1) + myFib(n)