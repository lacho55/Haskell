main :: IO()
main = do 
    print(highestCapital count1)


type Name = String
type Capital = Name
type AvgYearlyTemperature = Double
type Elevation = Int

data City = City Name Elevation AvgYearlyTemperature deriving(Read, Show)
data Country = Country Name Capital [City] deriving(Read, Show)

count1 :: [Country]
count1 = [(Country "France" "Paris" [(City "Verdun" 2 4), (City "Paris" 2 2)]),
          (Country "Germany" "Berlin" [(City "Baden Baden" 1 1), (City "Berlin" 1 1)])]

highestCapital :: [Country] -> Name
highestCapital allCountries = capWithMaxHight
    where
        listOfCapsAndEls =  [(countName, capName, citEl) | (Country countName capName listCities) <- allCountries, (City citName citEl _) <- listCities, capName == citName]
        maxHight = maximum [ x | (_, _,x) <- listOfCapsAndEls]
        capWithMaxHight = head [ x | (x, y, z) <- listOfCapsAndEls, z == maxHight]


