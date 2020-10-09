-- Алгебрични типове
-- изброени типове - конструкторите нямат аргументи
data Temp = Cold | Hot
data Season = Spring | Summer | Autumn | Winter

-- функции за работа с алгебрични типове - най-често ползваме pattern matching
weather :: Season -> Temp
weather Summer = Hot
weather _      = Cold

-- производни типове (резултатни типове, product types)
data People = Person Name Age

type Name = String
type Age = Int

--data Employee = Person Name Age String 
-- error: Multiple declarations of `Person' (не може да ползваме едно и също име на конструктор, дори и да е в различни типове, дори и да има различен брой аргументи)


-- Задача 0. Да се дефинира типа Shape с 4 конструктора: 
    -- Circle    - с 1 аргумент  - радиус
    -- Rectangle - с 2 аргумента - ширина и височина
    -- Triangle  - с 3 аргумента - 3 страни
    -- Cylinder  - с 2 аргумента - радиус на основата и височина
-- Типа Shape да се направи екземпляр на класа Show и за него да се дефинира метода show, позволяващ print-ването му.
data Shape = Circle Double | 
             Rectangle Double Double | 
             Triangle Double Double Double | 
             Cylinder Double Double 

circle, rectangle, cylinder, triangle :: Shape
circle    = Circle 3
rectangle = Rectangle 4 5
cylinder  = Cylinder 3 3
triangle  = Triangle 3 4 5

instance Show Shape where
    show (Circle radius)  = "A circle with radius " ++ show radius
    show (Rectangle a b)  = "A rectangle with sides: " ++ show a ++ " and " ++ show b
    show (Cylinder r h)   = "A cylinder with radius " ++ show r ++ " and height " ++ show h
    show (Triangle a b c) = "A triangle with sides: " ++ show a ++ ", " ++ show b ++ " and " ++ show c

-- Задача 1. За Shape да се дефинират:
-- a) функция perimeter, която намира периметъра на фигурата;
perimeter :: Shape -> Double
perimeter (Circle radius)  = 2 * pi * radius
perimeter (Rectangle a b)  = 2 * (a + b)
perimeter (Triangle a b c) = a + b + c
perimeter _                = error "Unsupported shape"
-- b) функция area, която намира лицето на фигурата;
area :: Shape -> Double
area (Circle radius)     = pi * radius * radius
area (Rectangle a b)     = a * b
area (Cylinder r h)      = 2 * (area base) + h * (perimeter base) 
    where base = Circle r -- основата на цилиндъра е кръг с радиус r
area tr@(Triangle a b c) = sqrt $ p * (p - a) * (p - b) * (p - c)
    where p = (perimeter tr) / 2
-- c) предикат isRound, който проверява дали дадена фигура е кръгла (някоя от стените ѝ е окръжност);
isRound :: Shape -> Bool
isRound (Circle _)     = True
isRound (Cylinder _ _) = True
isRound _              = False
-- d) предикат is2D, който проверява дали дадена фигура е равнинна (лежи в една равнина).
is2D :: Shape -> Bool
is2D (Cylinder _ _) = False
is2D _              = True

-- Задача 2. Да се дефинира функция sumArea, която приема списък от фигури и връща сумата от лицата на фигурите в списъка. 
sumArea :: [Shape] -> Double
--sumArea shapes = sum [area shape | shape <- shapes] -- вариант с list comprehension
sumArea shapes = foldr1 (+) (map area shapes)         -- вариант с map
--sumArea shapes = foldr1 (+) . map area shapes -- невалидно, защото map area lst се оценява до списък и не може да композираме функция със списък
--sumArea = foldr1 (+) . map area               -- безаргументен стил: валидно, защото всъщност е еквивалентно на sumArea lst = (foldr1 (+) . map area) lst

-- Задача 3. Да се дефинира функция biggestShape, която приема списък от фигури и връща тази с най-голямо лице.
biggestShape :: [Shape] -> Shape
biggestShape shapes = foldl1 (\ sh1 sh2 -> if area sh1 >= area sh2 then sh1 else sh2) shapes


-- Задача 4. Да се дефинира тип Point, който задава точка (в равнината или в пространството) и е екземпляр на Show.
-- Типа да се направи екземпляр на класа Eq и за него да се дефинира равенство на точки от една и съща размерност.
data Point = P2 Double Double | P3 Double Double Double deriving Show
-- чрез deriving Show ни се предоставя имплементация по подбразбиране на функцията show, която просто отпечатва имено на конструктора и стойностите на аргументите.
-- В някои случаи това е удобно, но при точките запис от вида "P3 x y z" може да не е особено подходящ.
-- Тази имплементация не може да бъде override-ната, затова ще си дефинираме наша функция за print-ване:
printPoint :: Point -> String
printPoint (P2 x y)   = "(" ++ show x ++ ", " ++ show y ++ ")"
printPoint (P3 x y z) = "(" ++ show x ++ ", " ++ show y ++ ", " ++ show z ++ ")"

instance Eq Point where
    (P2 x1 y1)    == (P2 x2 y2)    = x1 == x2 && y1 == y2
    (P3 x1 y1 z1) == (P3 x2 y2 z2) = x1 == x2 && y1 == y2 && z1 == z2
    _             == _             = error "Different point dimensions"


-- Задача 5. Да се дефинира функция distance за работа с типа Point, която намира разстоянието между две точки от една и съща размерност. 
-- Ако точките са с различна размерност (т.е. имат различен брой координати) функцията да връща съобщение за грешка.
distance :: Point -> Point -> Double
distance (P2 x1 y1)    (P2 x2 y2)    = sqrt $ (x2 - x1)^2 + (y2 - y1)^2
distance (P3 x1 y1 z1) (P3 x2 y2 z2) = sqrt $ (x2 - x1)^2 + (y2 - y1)^2 + (z2 - z1)^2
distance _             _             = error "Different point dimensions"

-- Задача 6. Да се дефинира функция getClosestPoint, която приема списък от точки ps и още една точка p. 
-- Като резултат функцията да връща тази точка от ps, която е най-близо до точката p.
getClosestPoint :: [Point] -> Point -> Point
getClosestPoint lst p = foldl1 (\ p1 p2 -> if distance p p1 <= distance p p2 then p1 else p2) lst


main :: IO()
main = do
    -- Function application $
    --print (sqrt 4 + 3 + 9)   -- -> 14.0 (sqrt(4) + 3 + 9 = 2 + 3 + 9 = 14)
    --print (sqrt (4 + 3 + 9)) -- -> 4.0  (sqrt(4+3+9) = sqrt(16) = 4)
    --print (sqrt $ 4 + 3 + 9) -- -> 4.0  (sqrt(4+3+9) = sqrt(16) = 4)
    --print $ 4 + 5            -- -> 9

    -- Задачи:
    --print circle    -- -> A circle with radius 3.0
    --print rectangle -- -> A rectangle with sides: 4.0 and 5.0
    --print cylinder  -- -> A cylinder with radius 3.0 and height 3.0
    --print triangle  -- -> A triangle with sides: 3.0, 4.0 and 5.0

    --print $ area (Circle 3)       -- -> 28.274333882308138
    --print $ area (Rectangle 4 5)  -- -> 20.0
    --print $ area (Cylinder 3 3)   -- -> 113.09733552923255
    --print $ area (Triangle 3 4 5) -- -> 6.0
 
    --print $ perimeter (Circle 3)       -- -> 18.84955592153876
    --print $ perimeter (Rectangle 4 5)  -- -> 18.0
    --print $ perimeter (Cylinder 3 3)   -- error: Unsupported shape
    --print $ perimeter (Triangle 3 4 5) -- -> 12.0
 
    --print $ isRound circle    -- -> True
    --print $ isRound rectangle -- -> False
    --print $ isRound cylinder  -- -> True
    --print $ isRound triangle  -- -> False
 
    --print $ is2D circle    -- -> True
    --print $ is2D rectangle -- -> True
    --print $ is2D cylinder  -- -> False
    --print $ is2D triangle  -- -> True
 
    --print $ sumArea [circle, rectangle, cylinder, triangle]      -- -> 167.3716694115407
    --print $ biggestShape [circle, rectangle, cylinder, triangle] -- -> A cylinder with radius 3.0 and height 3.0 (-> cylinder)


    --print (P2 0 1)              -- -> P2 0.0 1.0
    --print $ printPoint (P2 0 1) -- -> "(0.0, 1.0)"

    --print $ P2 0 1 == P2 0 0   -- -> False
    --print $ P2 0 1 == P2 0 1   -- -> True
    --print $ P2 0 1 /= P2 0 0   -- -> True
    --print $ P2 0 0 == P3 0 0 0 -- error: Different point dimensions
 
    --print $ distance (P2 0 0) (P2 1 1)     -- -> 1.4142135623730951
    --print $ distance (P3 1 1 1) (P3 2 2 2) -- -> 1.7320508075688772
    --print $ distance (P3 0 0 0) (P2 0 0)   -- error: Different point dimensions

    -- print $ "Closest point to point " ++ printPoint (P2 1 1) ++ " is point: " ++ (printPoint $ getClosestPoint [(P2 0 (-6)), (P2 2 3), (P2 10 4)] (P2 1 1))
    -- -> "Closest point to point (1.0, 1.0) is point: (2.0, 3.0)"

    --print $ getClosestPoint [(P2 0 (-6)), (P2 2 3), (P2 10 4)] (P3 1 1 1) -- error: Different point dimensions