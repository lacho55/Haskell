-- Варианти за Задача 1.
-- Задача 1. Да се дефинира функция rotate :: Int -> [a] -> [a], която получава цяло число n и списък xs и "завърта" xs с n позиции наляво, 
-- т.е. елементите на xs се преместват с n позиции наляво, като тези, които при преместването излизат извън списъка, се добавят на края му. 
-- При подаване на отрицателно число n, завъртането е надясно с абсолютната стойност на n.
rotate :: Int -> [a] -> [a]
rotate n xs = if n >= 0 then drop len xs ++ take len xs else rotate (len + length xs) xs
    where len = n `mod` length xs

-- Задача 2. Нека имаме следното представяне на двоично дърво от цели числа:
data BTree = Empty | Node Int BTree BTree
-- Казваме, че едно двоично дърво е огледално-симетрично, ако лявото му поддърво е огледален образ на дясното.
-- Да се дефинира предикат isSymmetric :: BTree -> Bool, който проверява дали дадено двоично дърво е огледално-симетрично.
isSymmetric :: BTree -> Bool
isSymmetric Empty               = True
isSymmetric (Node _ left right) = areMirrored left right
    where
        areMirrored :: BTree -> BTree -> Bool
        areMirrored Empty Empty                     = True
        areMirrored (Node x1 l1 r1) (Node x2 l2 r2) = x1 == x2 && areMirrored l1 r2 && areMirrored r1 l2

t4 :: BTree                         --   1
t4 = Node 1 (Node 2 Empty Empty)    --  / \
            (Node 3 Empty Empty)    -- 2   3

t5 :: BTree                                 --     1
t5 = Node 1 (Node 2 (Node 3 Empty Empty)    --    / \
                    Empty)                  --   2   2
            (Node 2 Empty                   --  /     \
                    (Node 3 Empty Empty))   -- 3       3

t6 :: BTree                                         --       1
t6 = Node 1 (Node 2 (Node 3 Empty Empty)            --    /     \
                    (Node 7 (Node 5 Empty Empty)    --   2       2
                            Empty))                 --  / \     / \
            (Node 2 (Node 7 Empty                   -- 3   7   7   3
                            (Node 5 Empty Empty))   --    /     \
                    (Node 3 Empty Empty))           --   5       5

-- Задача 3. Стандартните списъци в Haskell са хомогенни, т.е. съдържат елементи от един и същ тип. Нека дефинираме наш тип “вложен списък” NestedList, 
-- който може да съдържа както “обикновени” (“атомарни”) елементи, така и други списъци от типа NestedList. За леснота елементите ще са само цели числа.
data NestedList = Elem Int | List [NestedList]
-- Да се дефинира функция flatten :: NestedList -> [Int], която получава вложен списък list и го "изглажда", превръщайки го в стандартен, хомогенен списък, 
-- съдържащ числата от list.
flatten :: NestedList -> [Int]
flatten (Elem x)     = [x]
flatten (List lists) = concatMap flatten lists -- concatMap прилага функция върху елементите на списъка и конкатенира резултата
                                               -- еквивалентно на concat . map, но по-ефективно поради специфики в реализацията


main :: IO()
main = do
    --print $ rotate 5    ['a','b','c','d','e','f','g','h'] -- -> "fghabcde"
    --print $ rotate 7    ['a','b','c','d','e','f','g','h'] -- -> "habcdefg"

    --print $ rotate 8    ['a','b','c','d','e','f','g','h'] -- -> "abcdefgh"
    --print $ rotate (-8) ['a','b','c','d','e','f','g','h'] -- -> "abcdefgh"

    --print $ rotate 3    ['a','b','c','d','e','f','g','h'] -- -> "defghabc"
    --print $ rotate 11   ['a','b','c','d','e','f','g','h'] -- -> "defghabc"
    --print $ rotate (-5) ['a','b','c','d','e','f','g','h'] -- -> "defghabc"

    --print $ rotate (-2)  ['a','b','c','d','e','f','g','h'] -- -> "ghabcdef"
    --print $ rotate (-10) ['a','b','c','d','e','f','g','h'] -- -> "ghabcdef"
    

    --print $ isSymmetric t4 -- -> False
    --print $ isSymmetric t5 -- -> True
    --print $ isSymmetric t6 -- -> True
    

    --print $ flatten (List []) -- -> []
    --print $ flatten (Elem 1)  -- -> [1]

    print $ flatten (List [List [Elem 1, Elem 2], 
                           List [Elem 3, Elem 4], 
                           List [Elem 5, Elem 6]]) -- -> [1,2,3,4,5,6]
    print $ flatten (List [List [List [Elem 1], 
                                 List [Elem 2]], 
                           List [List [Elem 3], 
                                 List [Elem 4]],
                     List [List [List [Elem 5],
                                 List [Elem 6]]]]) -- -> [1,2,3,4,5,6]

    print $ flatten (List [Elem 1, 
                           List [Elem 2, 
                                 List [Elem 3, 
                                       Elem 4], 
                                 Elem 5]])         -- -> [1,2,3,4,5]
    print $ flatten (List [List [List [List [List [List [List [Elem 1]]]]]]]) -- -> [1]