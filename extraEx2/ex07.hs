import Data.List(nub)
main :: IO()
main = do
    print(coldestCapital count1)


type Name = String
type Capital = Name
type AvgYearlyTemperature = Double
type Elevation = Int
data City = City Name Elevation AvgYearlyTemperature deriving (Read, Show)
data Country = Country Name Capital [City] deriving (Read,Show)

count1 :: [Country]
count1 = [(Country "France" "Paris" [(City "Verdun" 2 4), (City "Paris" 2 2)]),
          (Country "Germany" "Berlin" [(City "Baden Baden" 1 1), (City "Berlin" 1 1)])]

coldestCapital :: [Country] -> Name
coldestCapital [] = error "There is no country!"
coldestCapital allCountries@( (Country name cap ((City cityName cityEv cityAbg) : cities) )   : otherCountries) = capWithMinTemp
    where
        listOfCapsAndTemps =  [(capName, citAvg) | (Country _ capName listCities) <- allCountries, (City citName _  citAvg) <- listCities, capName == citName]
        minTemp = minElem [temp | (_, temp) <- listOfCapsAndTemps] 
        capWithMinTemp = head [ x | (x, y) <- listOfCapsAndTemps, y == minTemp]


minElem::[Double]->Double
minElem [] = 0
minElem [x] = x
minElem (x:y:xs) 
 |x > y = minElem (y:xs)
 |x <= y = minElem (x:xs)
 |x == y = minElem (x:xs)
