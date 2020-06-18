main::IO()
main = do
    print(sumDigits 410)

interestNum :: Int -> Bool
interestNum n = n `mod` sumDigits n == 0

sumDigits :: Int -> Int
sumDigits num 
    | num <= 0 = num
    | otherwise = num `mod` 10 + sumDigits(num `div` 10)