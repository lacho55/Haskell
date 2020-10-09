import Data.List(nub)
import Data.Maybe(fromJust)

-- Решения на задачите от предния път за графи:

-- Задача 1. Да се дефинират следните функции за работа с графи, представени чрез списък от ребра:
-- а) nodes: по даден граф връща списък от върховете му
nodes :: [(Int, Int)] -> [Int]
nodes graph = nub $ foldr (\ (a, b) res -> a:b:res) [] graph
-- чрез fold-а ще превърнем списък от двойки в списък от елементите на двойките. Най-вероятно ще има повторения, така че с nub ги премахваме

nodes' :: [(Int, Int)] -> [Int]
nodes' = nub . foldr (\ (a, b) res -> a:b:res) [] -- безаргументен стил

-- б) neighbours: по даден граф и връх x, връща списък от съседните на х върхове
neighbours :: [(Int, Int)] -> Int -> [Int]
neighbours edges node = [b | (a, b) <- edges, a == node]

-- в) toAdjacencyList: по даден граф връща представянето му чрез списък на съседство 
-- (т.е. конструира списък с елементи от вида (x, [neighbours]), където x е връх в графа, а neighbours са неговите съседи).
toAdjacencyList :: [(Int, Int)] -> [(Int, [Int])]
toAdjacencyList edges = [(v, neighbours edges v) | v <- nodes edges]

-- Задача 2. Да се дефинира предикат isPath, който приема граф (зададен чрез списък на съседство) и списък от върхове и проверява дали списъкът представя път в графа.
isPath :: [(Int, [Int])] -> [Int] -> Bool
isPath _ []             = True           -- празен път
isPath graph [v]        = v `elem` nodes -- тривиален път - от един елемент
    where nodes = [v | (v, _) <- graph]
isPath graph (v1:v2:vs) = v2 `elem` (neighbours v1) && isPath graph (v2:vs)
    where neighbours v = fromJust (lookup v graph)
-- lookup е вградена функция за търсене в асоциативен списък - получава списък и ключ, и връща асоциацията на ключа.
-- Тъй като може ключа да не присъства (и следователно да няма асоциация), функцията всъщност не връща директно асоциацията, а асоциацията "опакована" чрез типа Maybe, 
-- който изглежда така: data Maybe a = Nothing | Just a. Така ако ключа не съществува, може да върнем "нищо" чрез Nothing, а иначе да върнем асоциацията опакована 
-- в Just. Така за да получим същинската асоциация ще трябва да я "разопаковаме" чрез функцията fromJust, която от (Just "нещо") ще ни върне самото "нещо"

-- втори вариант - ще ползваме техниката с zip, за да превърнем пътя от списък на върхове в списък на ребра,
-- и след това за всяко ребро от пътя ще проверим дали наистина е в графа
isPath' :: [(Int, [Int])] -> [Int] -> Bool
isPath' graph path = all isEdge edgesPath
    where
        edgesPath = zip path (tail path)
        isEdge (a, b) = b `elem` (fromJust $ lookup a graph)

-- А ако графът беше представен като списък от ребра, както в миналите задачи?
isPath'' :: [(Int, Int)] -> [Int] -> Bool
isPath'' graph path = all (`elem` graph) edgesPath
    where edgesPath = zip path (tail path)

graph1 :: [(Int, Int)]
graph1 = [(1, 2), (1, 3), (2, 3), (2, 4)]

graph2 :: [(Int, [Int])]
graph2 = [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])]

-- Упражнение 12

-- Асоциативен списък - списък от двойки (<ключ>, <асоциация>)
assocList :: [(Int, Char)]
assocList = [(1, 'a'), (2, 'b'), (3, 'c')]

-- Задача 1. Функция lookup' key dictionary, която търси key в асоциативния списък dictionary и връша първия му елемент с ключ равен на key.
-- Ако такъв ключ не присъства в списъка, да се върне грешка с error.
-- Името на функцията е с ' накрая, тъй като има вградена функция lookup.
lookup' :: Eq a => a -> [(a, b)] -> b
lookup' _ []                       = error "No such key"
lookup' key ((key', assoc) : rest) = if key == key' then assoc else lookup' key rest

-- А ако не искаме да връщаме грешка? Може да ползваме типа Maybe (data Maybe a = Nothing | Just a) по следния начин:
lookup'' :: Eq a => a -> [(a, b)] -> Maybe b
lookup'' _ []                       = Nothing -- aко не намерим ключа, връщаме Nothing
lookup'' key ((key', assoc) : rest) = if key == key' then Just assoc else lookup'' key rest -- иначе връщаме Just асоциацията. 
-- Така работи вграденият lookup
 

-- Задача 2. Функция replace xs dictionary, която връща като резултат списък, получен от xs, в който елементите са заменени с асоциацията им в dictionary.
replace :: Eq a => [a] -> [(a, b)] -> [b]
replace xs dictionary = map (\ x -> lookup' x dictionary) xs

-- Задача 3. Да се провери дали даден списък от точки от вида (х, у) принадлежи на графиката на функция, където х е по абсицата, а у по ординатата. 
-- За целта функцията да приема списък от точки и едноаргументна целочислена функция.
isGraph :: [(Int, Int)] -> (Int -> Int) -> Bool
isGraph points f = all isOnGraph points
    where isOnGraph (x, y) = f x == y


mat :: [[Int]]
mat = [[1,2,3],
       [4,5,6],
       [7,8,9],
       [10,11,12]]    

-- Задача 4. Да се дефинира функция transpose :: [[a]] -> [[a]], която получава матрица, представена като списък от списъци и я транспонира.
transpose :: [[a]] -> [[a]]
transpose []     = []
transpose ([]:_) = [] -- трябва да разгледаме този случай, за да си подсигурим че разглеждания ред е непразен, тъй като в следващия случай взимаме head
transpose xss    = map head xss : transpose (map tail xss) -- взимаме 1ви стълб, като вземем главите на всички редове
                                                           -- след това продължаваме с остатъка от матрицата - махаме първия ѝ стълб, 
                                                           -- като вземем опашките на всичките редове
                                                           
-- Задача 5. Да се дефинира функция rotate :: [[a]] -> [[a]], която завърта матрица на прав ъгъл по обратна на часовниковата стрелка посока
rotate :: [[a]] -> [[a]]
rotate = reverse . transpose

-- Задача 6. Да се дефинира функция spiral :: [[a]] -> [a], която обхожда матрица по спирала, започвайки от първия елемент на първия ред
-- и движейки се отвън навътре по посока на часовника до изчерпване на членовете, т.е.: 
-- 1ви ред, последна колона, последен ред наобратно, първа колона наобратно, втори ред, предпоследна колона, ...
spiral :: [[a]] -> [a]
spiral []       = []
spiral (xs:xss) = xs ++ spiral (rotate xss)


main :: IO()
main = do
    --print $ foldr (\ (a, b) res -> a:b:res) [] graph1 -- -> [1,2,1,3,2,3,2,4]
    --print $ nodes graph1 -- -> [1,2,3,4]

    --print $ neighbours graph1 2 -- -> [3,4]

    --print $ toAdjacencyList graph1 -- -> [(1,[2,3]),(2,[3,4]),(3,[]),(4,[])]

    --print $ isPath graph2 [1, 2, 4] -- -> True
    --print $ isPath graph2 [1, 3, 4] -- -> False
    --print $ isPath graph2 [2, 3]    -- -> True
    --print $ isPath graph2 [3, 1]    -- -> False

    --print $ isPath' graph2 [1, 2, 4] -- -> True
    --print $ isPath' graph2 [1, 3, 4] -- -> False
    --print $ isPath' graph2 [2, 3]    -- -> True
    --print $ isPath' graph2 [3, 1]    -- -> False

    --print $ isPath'' graph1 [1, 2, 4] -- -> True
    --print $ isPath'' graph1 [1, 3, 4] -- -> False
    --print $ isPath'' graph1 [2, 3]    -- -> True
    --print $ isPath'' graph1 [3, 1]    -- -> False


    --print assocList -- -> [(1,'a'),(2,'b'),(3,'c')]

    --print $ lookup' 2 assocList  -- -> 'b'
    --print $ lookup' 4 assocList  -- No such key ... error, called at ...
    --print $ lookup'' 2 assocList -- -> Just 'b'
    --print $ lookup'' 4 assocList -- -> Nothing

    --print $ replace [1,2,3,4,5] assocList -- No such key ... error, called at ...
    --print $ replace [1,2,3] assocList     -- -> "abc"


    --print $ isGraph [(1, 1), (2, 4), (3, 9)] (\ x -> x * x) -- -> True
    --print $ isGraph [(1, 1), (2, 4), (3, 6)] (\ x -> x * x) -- -> False


    --print $ transpose mat -- -> [[1,4,7,10],
                          --     [2,5,8,11],
                          --     [3,6,9,12]]
                    
    --print $ rotate mat    -- -> [[3,6,9,12],
                          --     [2,5,8,11],
                          --     [1,4,7,10]]
                 
    --print $ spiral mat   -- -> [1,2,3,6,9,12,11,10,7,4,5,8]