main :: IO()
main = do
    print (function myL)

sumByRule :: Int -> Int -> Int
sumByRule a b = sum (filter (\ x -> ((x - 1) `mod` 4 == 0) && (6 `elem` (digits x))) [a..b])
    where  
        digits 0 = []
        digits x = digits (x `div` 10) ++ [x `mod` 10]


