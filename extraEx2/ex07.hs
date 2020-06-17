main :: IO()
main = do
    print(1)


type Name = String
type Capital = Name
type AvgYearlyTemperature = Double
type Elevation = Int
data City = City Name Elevation AvgYearlyTemperature deriving (Read, Show)
data Country = Country Name Capital [City] deriving (Read,Show)

coldestCapital :: [Country] -> Name





--Дефинирайте функция coldestCapital :: [Country] -> Name, която получава като аргумент списък
--от държави и връща като резултат името на държавата от списъка с най-студена столица
--(столица с най-ниска средногодишна температура).