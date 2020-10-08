import Data.List(nub)

-- от Упражнение 7:
-- Задача 1. Да се дефинира функция insert :: Int -> [Int] -> [Int], която добавя елемент в сортиран списък, като резултатният списък също е сортиран
insert :: Int -> [Int] -> [Int]
insert x [] = [x]
insert x xs@(y:ys) = if x < y then x : xs else y : (insert x ys)

-- Задача 2. Да се реализира функция insertionSort :: [Int] -> [Int], която реализира сортиране чрез вмъкване върху списък
insertionSort :: [Int] -> [Int]
insertionSort xs = foldr insert [] xs

-- Задача 3. Да се дефинира функция closestPoint ps, която приема списък ps от точки в равнината (представени чрез двойки (x, y)) 
-- и връща едноаргументна функция, чиято стойност в дадена точка p e най-близката до p точка от списъка ps.
type Point = (Double, Double)

closestPoint :: [Point] -> (Point -> Point)
closestPoint ps (px, py) = foldr1 chooseCloser ps
    where
        chooseCloser p1 p2 = if (distanceToP p1) < (distanceToP p2) then p1 else p2
        distanceToP (x, y) = sqrt ((px - x) ^ 2 + (py - y) ^ 2)


-- Упражнение 8:
-- Задача 1. Да се дефинира тип Product, определящ се от име, количество и цена. 
-- Да се дефинира тип Shop (“база от данни”), който представлява инвентара от продукти на даден магазин.
type Product = (String, Int, Float)
type Shop = [Product]

p1, p2, p3, p4 :: Product
p1 = ("milk", 5, 1.20)
p2 = ("cheese", 20, 1.80)
p3 = ("bread", 10, 0.50)
p4 = ("chocolate", 3, 2.00)

shop :: Shop
shop = [p1, p2, p3, p4]

-- Задача 2. Да се дефинира функция getPrice, която връща цената на даден продукт.
getPrice :: Product -> Float
getPrice (_, _, price) = price

-- Задача 3. Да се дефинира функция getTotal, която връща оборота на даден магазин, ако е продаден целият инвентар.
getTotal :: Shop -> Float
getTotal [] = 0
getTotal ((_, quantity, price) : xs) = fromIntegral quantity * price + getTotal xs

getTotal' :: Shop -> Float
getTotal' shop = sum (map getPriceWithQuantity shop)
    where 
        getPriceWithQuantity (_, quantity, price) = (fromIntegral quantity) * price

-- Задача 4. Да се дефинира функция buy, която симулира “закупуването” на даден продукт, като приема име, количество и магазин
-- Да се вземе предвид, че не може след продажбата в магазина да имаме отрицателно количество за даден продукт. 
-- Ако искаме да купим продукт, но неговата наличност е недостатъчна, нека операцията да е празна, т.е. да не променя нищо.
-- Ако след покупка количеството е станало 0, продуктът да се премахне от инвентара.
buy :: String -> Int -> Shop -> Shop
buy _ _ [] = error "No such product"
buy name' quantity' (x@(name, quantity, price) : xs)
    | name' == name && quantity' < quantity  = (name, quantity - quantity', price) : xs -- намаляме количеството
    | name' == name && quantity' == quantity = xs                                       -- премахваме продукта изцяло
    | name' == name && quantity' > quantity  = x : xs                                   -- не променяме нищо
    | otherwise                              = x : buy name' quantity' xs               -- продължаваме да търсим в магазина

-- Задача 5. Да се дефинира функция getNeeded, която за даден магазин връща списък от продукти, чиято наличност е по-малка или равна на даден праг (количество).
getNeeded :: Int -> Shop -> [Product]
getNeeded _ [] = []
getNeeded needed (x@(name, quantity, price) : xs) -- @ е начин за създаване на синоними, т.е. тук x и векторът (name, quantity, price) са 2 имена на една и съща стойност
    | quantity <= needed = x : getNeeded needed xs
    | otherwise          = getNeeded needed xs

getNeeded' :: Int -> Shop -> [Product]
getNeeded' needed xs = filter ((<= needed) . getQuantity) xs
    where
        getQuantity :: Product -> Int
        getQuantity (_, q, _) = q

getNeeded'' :: Int -> Shop -> [Product]
getNeeded'' needed xs = [x | x@(_, quantity, _) <- xs, quantity <= needed]

-- Задача 6. Да се дефинира функция getAverage, която която връща средната цена на продуктите на даден магазин.
getAverage :: Shop -> Float
getAverage xs = sum prices / fromIntegral (length prices)
    where prices = [price | (_, _, price) <- xs]

getAverage' :: Shop -> Float
getAverage' xs = sum (map getPrice xs) / fromIntegral (length xs)

-- В тези две решения не отчитаме количеството на продуктите - смятаме просто средната стойност на самите цени.
-- Ако искаме да сметнем средната стойност, отчитайки и количеството (т.е. цените на продукти с по-голямо количество да са с по-голяма тежест), може да направим така:
getAverageAlt :: Shop -> Float
getAverageAlt xs = prices / fromIntegral quantities
    where
        prices = sum [fromIntegral quantity * price | (_, quantity, price) <- xs]
        quantities = sum [quantity | (_, quantity, _) <- xs]


-- Задача 7. Да се дефинира функция closestToAverage, която намира името на продукта, чиято цена е най-близка до средната за всички в даден магазин.
closestToAverage :: Shop -> String
closestToAverage xs = name
    where 
        (name, _, _) = foldl1 compareProducts xs -- тъй като резултатът от функцията е тройка, а на нас ни трябва само първата ѝ координата,
                                                 -- си запазваме резултатът тук и извличаме само това, което ни е нужно от него
        compareProducts p1@(_, _, price1) p2@(_, _, price2) = if abs (price1 - average) < abs (price2 - average) then p1 else p2
        average = getAverage xs

closestToAverage' :: Shop -> String
closestToAverage' xs = name
    where 
            substract a (x, y, z) = (x, y, abs (z - a))
            compare p1@(_, _, diff1) b@(_, _, diff2) = if diff1 <= diff2 then p1 else p2 -- ще взимаме тази тройка, която е с по-малка трета координата
            (name, _, _) = foldl1 compare (map (substract (getAverage xs)) xs) 
            -- чрез map-ването ще заменим всяка цена с разликата между нея и средната, а чрез fold-ването ще намерим "минималната" тройка 
            -- в смисъла на горе дефинираното сравнение, т.е. тройката с най-малка трета координата (най-малка разлика между цената ѝ и средната цена)
            -- или с други думи: продукта, чиято цена е най-близка до средната. Ние търсим името на точно този продукт

-- Задача 8. Да се дефинира функция cheaperAlternative, която намира броя на продуктите, за които има продукт със същото име, но по-ниска цена.
cheaperAlternative :: Shop -> Int
cheaperAlternative xs = length (filter hasTwoPrices (groupPrices xs))
    where
        names = nub [name | (name, _, _) <- xs]
        -- nub е вградена ф-ия, която изтрива повторенията в списък. Тук ще получим списък от имената, където всяко се среща само веднъж.

        groupPrices :: Shop -> [[Float]]
        groupPrices xs = [[price | (name', _, price) <- xs, name' == name] | name <- names]
        -- тук имаме вложени list comprehension-и. Може да направим аналогия с вложени цикли - външния е за имената, вътрешния е за самите продукти.
        -- Така за всяко име ще обходим списъка с продукти и ще конструираме списък от цените на тези продукти, които имат това име, т.е.
        -- groupPrices ще конструира списъци от различните цени на един и същи продукт
        -- например в shop2 ще върне [[1.0,1.0],[2.5,5.0],[2.3]], т.к. bread присъства 2 пъти с цена 1, cheese - 2 пъти с цени 2.5 и 5.0, butter - веднъж с 2.3  
        
        hasTwoPrices xs = length (nub xs) > 1
        -- ако има две различни цени, то след като премахнем повторенията трябва списъкът да е с дължина поне 2


shop1, shop2 :: Shop
shop1 = [("bread", 5, 1), ("milk", 1, 2.5),   ("lamb", 1, 10), ("cheese", 1, 5), ("butter", 1, 2.3)]
shop2 = [("bread", 1, 1), ("cheese", 1, 2.5), ("bread", 1, 1), ("cheese", 1, 5), ("butter", 1, 2.3)]

main :: IO()
main = do
    --print (insert 1 [2,3,4]) -- -> [1,2,3,4]
    --print (insert 3 [1,2,4]) -- -> [1,2,3,4]
    --print (insert 4 [1,2,3]) -- -> [1,2,3,4]
    --print (insertionSort [7,3,2,5,1,6,10,8,9,4]) -- -> [1,2,3,4,5,6,7,8,9,10]

    --print ((closestPoint [(0, 0), (1, 1), (2, 2)]) (0.9, 2)) -- -> (1.0, 1.0)
    --print ((closestPoint [(0, 0), (1, 1), (2, 2)]) (1, 2))   -- -> (2.0, 2.0) 
    --print ((closestPoint [(0, 0), (2, 2), (1, 1)]) (1, 2))   -- -> (1.0, 1.0)
    -- Забележка: В списъка има две точки, които са на минималното разстояние от (1.0, 2.0) - това са (2.0, 2.0) и (1.0, 1.0)
    -- Когато ползваме (<) в chooseCloser, при равноотдалечени точки влизаме в else частта и връщаме втората, 
    -- т.е. накрая ще върнем тази измежду точките на минимално разстояние, която е последна в списъка.
    -- Ако ползваме (<=) обаче, при равноотдалечени точки ще върнем първата, и така накрая ще върнем първата измежду точките на минимално разстояние.


    --print (getPrice ("milk", 5, 1.20)) -- -> 1.2
    --print (getTotal shop) -- -> 53.0

    --print (buy "milk" 3 shop) -- -> [("milk",2,1.2),("cheese",20,1.8),("bread",10,0.5),("chocolate",3,2.0)]
    --print (buy "milk" 5 shop) -- -> [("cheese",20,1.8),("bread",10,0.5),("chocolate",3,2.0)]

    --print (getNeeded 5 shop)  -- -> [("milk",5,1.2),("chocolate",3,2.0)]

    --print (getAverage shop1)    -- -> 4.16
    --print (getAverageAlt shop1) -- -> 2.7555554

    --print (closestToAverage shop1)  -- -> "cheese"
    --print (closestToAverage' shop1) -- -> "cheese"

    --print (cheaperAlternative shop2) -- -> 1 (поради cheese, което присъства с цени 2.5 и 5.0)