main::IO()
main = do
    print(generate 1 3) --TASK01
    print(listSquares 1 30) --TASK02
    print(listSquares 250 300)
    print(splitPoints (1,1) 5 [(1,2),(2,3),(10,15),(-1,1),(12,14)]) --TASK03
    print(getAverageBalance (as,ps) (\ (_,_,city) -> city == "Burgas")) --TASK04
    print(getAverageBalance (as,ps) (\ (_,(n:_),_) -> n == 'P')) --TASK04

--TASK01
func :: Double -> Int -> Double
func _ 1 = 1
func p n = ((func p (n - 1)) + (1 / ((fromIntegral n) ** p)))

generate :: Double -> Int -> [Double]
generate p n = [(func p k) | k<-[1..n]]

--TASK02
--Defining our own isSquare function in order to avoid the imported one(not sure if it is allowed or not)
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

--Making a list from all the divisors of n
divisors :: Int -> [Int]
divisors n = [x * x | x <- [1..(n)], n `rem` x == 0]

listSquares :: Int -> Int ->[(Int, Int)]
listSquares 0 _ = error "a has to be greater than 0"
listSquares _ 0 = error "b has to be greater than 0"
listSquares a b = [(i,sum(divisors i)) | i <- filter isSquare [a..b] ]

--TASK03
type Point = (Double, Double)

splitPoints :: Point -> Double -> [Point] -> ([Point], [Point])
splitPoints (x, y) r ps = (filter isInsideCircle ps, filter (not.isInsideCircle) ps)
    where
        distance ::  Point -> Point -> Double
        distance (x1,y1) (x2,y2) = sqrt((x1-x2)**2 + (y1-y2)**2)

        isInsideCircle ::  Point -> Bool
        isInsideCircle (xc,yc)
            | (distance (xc,yc) (x,y) < r) = True
            | (distance (xc,yc) (x,y) >= r) = False

--TASK04
type Account = (Int, Int, Double)
type Person = (Int, String, String)

ps :: [Person]
ps = [(1, "Ivan", "Sofia"), (2, "Georgi", "Burgas"),
 (3, "Petar", "Plovdiv"), (4, "Petya", "Burgas")]
 
as :: [Account]
as = [(1, 1, 12.5), (2, 1, 123.2), (3, 2, 13.0), (4, 2, 50.2),
 (5, 2, 17.2), (6, 3, 18.3), (7, 4, 19.4)]


getAverageBalance :: ([Account], [Person]) -> (Person -> Bool) -> Double
getAverageBalance ( (idAc, idPerson, balance) : as, (idPerson', name, city) : ps) p = sum [balance| (_, idPerson,balance) <- as, elem idPerson (getIds getNeededPeope)] / fromIntegral lengthOfBalance
    where
        getNeededPeope = filter p ps
        getIds idsOfPeople = [id | (id, _, _) <- idsOfPeople]
        lengthOfBalance = length [balance | (_, idPerson, balance) <- as, elem idPerson (getIds getNeededPeope)]
