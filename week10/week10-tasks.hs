-- Упражнение 10: Рекурсивни алгебрични типове. Дървета

-- Задача 0. Да се дефинира рекурсивен алгебричен тип двоично дърво (BTree), който има стойности от тип Int по върховете си. Нека BTree е екземпляр на класа Show.
{- data BTree = Empty | Node Int BTree BTree deriving Show

t1 :: BTree                                 --    5
t1 = Node 5 (Node 2 Empty                   --   / \
                    (Node 3 Empty Empty))   --  2   6
            (Node 6 Empty Empty)            --   \
                                            --    3    -}

-- Задача 1. Да се преработи алгебричния тип BTree, така че при конструирането му да може да се определя типа на възлите.
data BTree a = Empty | Node a (BTree a) (BTree a) deriving Show

t1 :: BTree Int                             --    5
t1 = Node 5 (Node 2 Empty                   --   / \
                    (Node 3 Empty Empty))   --  2   6
            (Node 6 Empty Empty)            --   \
                                            --    3 

t2 :: BTree Int                             --    5
t2 = (Node 5 (Node 3 Empty Empty)           --   / \
             (Node 4 (Node 5 Empty Empty)   --  3   4
                     (Node 7 Empty Empty))) --     / \
                                            --    5   7

charTree :: BTree Char                      --   a
charTree = Node 'a' (Node 'b' Empty Empty)  --  / \
                    (Node 'c' Empty Empty)  -- b   c

t3 :: BTree Int                             --     1     
t3 = Node 1 (Node 2 (Node 5 Empty Empty)    --    / \    
                     Empty)                 --   2   3 
            (Node 3 (Node 7 Empty Empty)    --  /   / \  
                    (Node 6 Empty Empty))   -- 5   7   6 


-- За двоичното дърво да се дефинират следните функции:
-- a) size, която намира броя на възлите на двоично дърво;
size :: BTree a -> Int
size Empty               = 0
size (Node _ left right) = 1 + size left + size right
-- b) height, която намира височината на двоично дърво;
height :: BTree a -> Int
height Empty               = 0
height (Node _ left right) = 1 + max (height left) (height right)
-- c) sumTree, която намира сумата от възлите на двоично дърво;
-- Забележка: функцията трябва да работи само за такива дървета, чиито възли наистина могат да се сумират! (не би трябвало да работи например за BTree Char)
sumTree :: Num a => BTree a -> a
sumTree Empty               = 0
sumTree (Node x left right) = x + sumTree left + sumTree right
-- d) sumLeaves, която намира сумата елементите по листата на двоично дърво;
sumLeaves :: Num a => BTree a -> a
sumLeaves Empty                = 0
sumLeaves (Node x Empty Empty) = x
sumLeaves (Node _ left  right) = sumLeaves left + sumLeaves right
-- e) inorder, която обхожда двоично дърво в ред Ляво-Корен-Дясно;
inorder :: BTree a -> [a]
inorder Empty               = []
inorder (Node x left right) = inorder left ++ [x] ++ inorder right
-- по аналогичен начин могат да се реализират и другите обхождания на дърво (Корен-Ляво-Дясно, Ляво-Дясно-Корен, Дясно-Корен-Ляво, ...)

-- представяне на аритметичен израз чрез дърво - по листата ще има стойности (числа, неизвестни), 
-- а по вътрешните върхове - инфиксни операции (+, -, *, /, ^)
expression :: BTree Char                                            --      -
expression = Node '-' (Node '+' (Node '5' Empty Empty)              --     / \
                                (Node '*' (Node '2' Empty Empty)    --    +   4
                                          (Node 'x' Empty Empty)))  --   / \
                      (Node '4' Empty Empty)                        --  5   *
                                                                    --     / \
getExpression :: BTree Char -> String                               --    2   x
getExpression Empty                = ""
getExpression (Node c Empty Empty) = [c]
getExpression (Node c left right)  = "(" ++ (getExpression left) ++ [c] ++ (getExpression right) ++ ")"
-- това е точно inorder обхождане, с добавени скоби

-- f) равенство на дървета (чрез операцията (==))
instance Eq a => Eq (BTree a) where
    Empty         == Empty         = True
    Node x1 l1 r1 == Node x2 l2 r2 = x1 == x2 && l1 == l2 && r1 == r2

-- Задача 2. Да се дефинира функция average, която приема двоично дърво от цели числа и пресмята средно-аритметичното от записаното във върховете му.
-- 1ви вариант
average :: BTree Int -> Double
average tree = fromIntegral (sumTree tree) / fromIntegral (size tree) -- тук ще обходим цялото дърво 2 пъти - 1во за да сумираме възлите, 2ро за да намерим броя им
-- 2ри вариант
average' :: BTree Int -> Double 
average' tree = fromIntegral (sum nodes) / fromIntegral (length nodes)
    where nodes = inorder tree -- тук ще обходим дървото само веднъж - за да конструираме списъка nodes, но после ще обхождаме резултатния списък 2 пъти. 

-- Задача 3. Да се дефинира функция getLevel, която приема цяло число k и двоично дърво t и връща списък от елементите на k-то ниво на t.
getLevel :: Int -> BTree a -> [a]
getLevel _ Empty               = []
getLevel 1 (Node x left right) = [x]
getLevel k (Node _ left right) = getLevel (k-1) left ++ getLevel (k-1) right 

-- Задача 4. Да се дефинира функцията getLevelsTree, която приема двоично дърво от произволен тип
-- и заменя всеки негов възел с двойка от стойността на възела и номера на нивото му.
-- Например:
--     1             (1,0)
--    / \             / \
--   2   3   =>   (2,1) (3,1)
--  /   / \       /     /  \
-- 5   7   6    (5,2) (7,2) (6,2)

getLevelsTree :: BTree a -> BTree (a,Int)
getLevelsTree bt = helper bt 0
    where 
        helper Empty               _   = Empty
        helper (Node x left right) lvl = Node (x, lvl) (helper left (lvl + 1)) (helper right (lvl + 1))

-- Задача 5. Да се дефинира функция mirrorTree, която преобразува двоично дърво в "огледалното" му.
-- Например:
--     1           1    
--    / \         / \
--   2   3   =>  3   2
--  /   / \     / \   \
-- 5   7   6   6   7   5

mirrorTree :: BTree a -> BTree a
mirrorTree Empty               = Empty
mirrorTree (Node x left right) = Node x (mirrorTree right) (mirrorTree left)

-- Задача 6. Да се дефинира функция mapTree, която приема функция f и двоично дърво t и прилага f към всеки възел на t.
mapTree :: (a -> b) -> BTree a -> BTree b
mapTree _ Empty               = Empty
mapTree f (Node x left right) = Node (f x) (mapTree f left) (mapTree f right)


main :: IO()
main = do
    --print t1 -- -> Node 5 (Node 2 Empty (Node 3 Empty Empty)) (Node 6 Empty Empty)

    --print $ size t1       -- -> 4
    --print $ height t1     -- -> 3

    --print $ sumTree t1    -- -> 16
    --print $ sumTree charTree -- No instance for (Num Char) arising from a use of `sumTree'
    --print $ sumLeaves t1  -- -> 9

    --print $ inorder t2    -- -> [3,5,5,4,7]
    --print $ getExpression expression -- -> "((5+(2*x))-4)"

    --print $ t1 == t1   -- -> True
    --print $ t1 == t2   -- -> False

    --print $ average t2    -- -> 4.8 ((3 + 5 + 5 + 4 + 7) / 5)

    --print $ getLevel 3 t2 -- -> [5,7]
    --print $ getLevelsTree t3 
    {- -> Node (1,0) (Node (2,1) (Node (5,2) Empty Empty) 
                                 Empty) 
                     (Node (3,1) (Node (7,2) Empty Empty) 
                                 (Node (6,2) Empty Empty)) -}

    --print $ mirrorTree t3
    {- -> Node 1 (Node 3 (Node 6 Empty Empty)                          
                         (Node 7 Empty Empty)) 
                 (Node 2 Empty 
                         (Node 5 Empty Empty)) -}

    --print $ mapTree (\ x -> x * x) t3
    {- --> Node 1 (Node 4 (Node 25 Empty Empty) 
                          Empty) 
                  (Node 9 (Node 49 Empty Empty) 
                          (Node 36 Empty Empty)) -}