import Data.List(delete)
-- Упражнение 11

data BTree a = Empty | Node a (BTree a) (BTree a) deriving Show
-- Задача 1. Нека имаме типът за цвят 
data Color = Red | Green | Blue deriving (Read, Show, Eq)
-- Дефинирайте функция maxDepthBlueNode, която намира дълбочината на най-дълбокия (най-отдалечения от корена) връх с цвят Blue на дадено двоично дърво от тип Color.

colorTree :: BTree Color                                            --        Blue
colorTree = Node Blue (Node Red (Node Green Empty Empty)            --       /    \
                                Empty)                              --    Red      Red
                      (Node Red (Node Blue (Node Green Empty Empty) --    /        /
                                           (Node Red Empty Empty))  -- Green     Blue
                                Empty)                              --           /   \
                                                                    --        Green  Red

maxDepthBlueNode :: BTree Color -> Int
maxDepthBlueNode tree  = helper tree 1
    where 
        helper Empty                  _         = 0
        helper (Node Blue left right) currDepth = maximum [currDepth, helper left (currDepth + 1), helper right (currDepth + 1)]
        helper (Node _    left right) currDepth = max (helper left (currDepth + 1)) (helper right (currDepth + 1))
-- max e функция на два аргумента. За да намерим максимума на повече елементи ползвайки max, трябва да ползваме вложени извиквания, например:
-- helper (Node Blue left right) currDepth = max currDepth (max (helper left (currDepth + 1)) (helper right (currDepth + 1)))
-- В горното решение просто поставяме елементите в списък, и извикваме maximum - функция, която приема само един списък и намира максималния елемент в него

-- Задача 2. Дефинирайте функция maxDepthColorNode, която намира дълбочината на най-дълбокия връх с цвят, подаден като аргумент на дадено двоично дърво от тип Color. 
maxDepthColorNode :: BTree Color -> Color -> Int
maxDepthColorNode tree color = helper tree 1
    where 
        helper Empty _ = 0
        helper (Node c left right) currDepth 
            | c == color = maximum [currDepth, helper left (currDepth + 1), helper right (currDepth + 1)]
            | otherwise  = max (helper left (currDepth + 1)) (helper right (currDepth + 1))

-- Задача 3. Да се дефинира алгебричен тип NTree а, който да представлява дърво с произволен брой наследника на всеки възел.
-- За него да се дефинира функция nTreeSize, която брои елементите му.
data NTree a = NEmpty | NNode a [(NTree a)]  
t4 :: NTree Int                               --      1
t4 = NNode 1 [(NNode 2 [(NNode 3 [NEmpty]),   --     / \
                        (NNode 4 [NEmpty]),   --    2   6
                        (NNode 5 [NEmpty])]), --   /|\  |
              (NNode 6 [(NNode 7 [NEmpty])])] --  3 4 5 7

nTreeSize :: NTree a -> Int
nTreeSize NEmpty             = 0
nTreeSize (NNode _ subTrees) = 1 + sum (map nTreeSize subTrees)


-- Друго представяне на дървета с произволен брой наследници: 
-- Асоциативен списък [(а, [а])], където a е типът на върховете на дървото. 
-- Ключове в списъка са върховете на дървото, а асоциираната с даден ключ стойност е списък от синовете на съответния връх (подобно на представянето на графите в Тема 10 от лекциите).

-- I. Асоциативен списък, описващ преките наследници (синовете) на върховете, които не са листа.
t5 :: [(Int, [Int])]
t5 = [(4, [2, 5]), (2, [1, 3])]
{-   4
    / \
   2   5
  / \
 1   3   -}

-- Задача 4. Да се дефинира функция twoChildrenNodes, която намира броя на върховете в дърво, които имат точно два наследника. 
hasTwoChildren :: (Int, [Int]) -> Bool
hasTwoChildren (_, children) = length children == 2

twoChildrenNodes :: [(Int, [Int])] -> Int
twoChildrenNodes nodes = length $ filter hasTwoChildren nodes

-- Задача 5. Да се дефинира функция allHaveTwoChildren, която проверява дали всички върхове (които не са листа) в дървото имат точно по два наследника.
allHaveTwoChildren :: [(Int, [Int])] -> Bool
allHaveTwoChildren nodes = foldr1 (&&) (map hasTwoChildren nodes) 
-- чрез map-ването получаваме списък от булеви стойности, след което fold-ваме с логическо и - ако всичките са True, резултата на fold-а също ще е True

-- втори вариант
allHaveTwoChildren' :: [(Int, [Int])] -> Bool
allHaveTwoChildren' nodes = all hasTwoChildren nodes


-- II. Асоциативен списък, описващ преките наследници (синовете) на всички върхове (включително и листата - за тях списъкът от наследници е празен).
t6 :: [(Int, [Int])]
t6 = [(1, [2, 3, 4]), (2, [5, 6]), (3, [7]), (4, [8, 9]), (5, []), (6, [10]), (7, []), (8, []), (9, []), (10, [])]
{-     
       1 
  2    3    4
 5 6   7   8 9
    10          -}

-- Задача 6. Дефинирайте функция findUncles, която за дадени дърво tree и връх node на tree намира списък от всички чичовци (братя на бащата) на node в tree.
findUncles :: [(Int, [Int])] -> Int -> [Int]
findUncles tree node = if null parent then [] else brothers (head parent)
    where 
        parent = [v | (v, vs) <- tree, elem node vs] -- бащата на node е ключът на двойката, в която node присъства като елемент на списъка от наследници
        brothers v = concat [delete v vs | (_, vs) <- tree, elem v vs]
        -- тук очакваме всеки връх да е уникален (т.е. да е уникално цяло число), така че v ще се среща само веднъж
        -- delete е вградена функция от Data.List, която премахва първото срещане на даден елемент в списък. Така премахваме бащата, за да получим само братята му
        -- тъй като ползваме list comprehension, всъщност получаваме списък от списъци (който съдържа само един елемент), затова се налага с concat да го превърнем
        -- в обикновен списък


main :: IO()
main = do 
    --print $ maxDepthBlueNode colorTree        -- -> 3
    --print $ maxDepthColorNode colorTree Red   -- -> 4
    --print $ maxDepthColorNode colorTree Green -- -> 4
    --print $ maxDepthColorNode colorTree Blue  -- -> 3
 
    --print $ nTreeSize t4 -- -> 7
 
    --print $ twoChildrenNodes [(4, [2, 5]), (2, [1, 3])] -- -> 2 (възлите 4 и 2)
    --print $ twoChildrenNodes [(10, [3, 7, 12]), (3, [5, 8, 9]), (7, [11, 13]), (12, [6, 4]), (8, [1, 2])] -- -> 3 (възлите 7, 12 и 8)
 
    --print $ allHaveTwoChildren' [(10, [3, 7, 12]), (3, [5, 8, 9]), (7, [11, 13]), (12, [6, 4]), (8, [1, 2])] -- -> False
    --print $ allHaveTwoChildren' [(4, [2, 5]), (2, [1, 3])] -- -> True
 
    --print $ findUncles t6 5  -- -> [3,4]
    --print $ findUncles t6 7  -- -> [2,4]
    --print $ findUncles t6 10 -- -> [5]
    --print $ findUncles t6 2  -- -> []