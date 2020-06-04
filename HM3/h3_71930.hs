import Data.List(nub)
import Data.List

main::IO()
main = do
    --TASK01
    print (containsWord t1 "acd")
    print (containsWord t1 "cd")
    print (containsWord t1 "ab")
    print (containsWord t1 "af")
    print (containsWord t1 "ae")

    --TASK02
    print (genWords t1)

    --TASK03
    print (allContain [t1, t2])

    --TASK04
    --print (isGraceful t3)

    --TASK05
    print (isBinarySearchTree t5)
    print (isBinarySearchTree t6)
    print (isBinarySearchTree t7)
    
    

--TASK01
data BTree a = Empty | Node a (BTree a) (BTree a) deriving Show

t1 :: BTree Char                                 --     a   
t1 = Node 'a' (Node 'c' (Node 'f' Empty Empty)   --    / \ 
                        (Node 'd' Empty Empty))  --   c   b
              (Node 'b' Empty                    --  / \    \
                        (Node 'e' Empty Empty))  -- f   d    e

     
containsWord :: BTree Char -> String -> Bool
containsWord tree [] = False
containsWord tree@(Node el left right) str@(x:xs) = elem str (genWords tree)


--TASK02
genWords :: BTree Char -> [String] 
genWords (Empty) = []
genWords (Node current Empty Empty) = [[current]]
genWords tr@(Node current left right) = results (map (current:) subPaths ++ subPaths)
        where
            subPaths = genWords right ++ genWords left
            subTr = subTrees tr
            results :: [String] -> [String]
            results str = [x | x <-str, y <-subTr, checksPath y x]


--Generating a list of all subtrees of a binary tree
subTrees :: BTree a -> [BTree a]
subTrees    Empty         = []
subTrees tree@(Node _ left right) = tree : (subTrees left ++ subTrees right)

--A Function that checks if there is such path in the tree
checksPath::BTree Char ->String->Bool
checksPath Empty "" = True
checksPath Empty (x:xs) = False
checksPath (Node current (left) (right)) "" = True
checksPath (Node current (left) (right)) (x:xs) =  if current == x
                                        then checksPath left xs || checksPath right xs
                                        else False


--TASK03
t2 :: BTree Char                                    -- a
t2 = Node 'a' (Node 'c' (Node 'd' Empty Empty)     -- / \
                        Empty)                    -- c   b
              (Node 'b' Empty Empty)             -- /
                                                -- d


allContain :: [BTree Char] -> [String]
allContain tr@(x:xs) = listOfEqualWords
    where
        listOfAllWords = [genWords x | x <- tr]
        listOfEqualWords = last [same (head listOfAllWords) x | x <-listOfAllWords]

same [] _ = []
same _ [] = []
same (x:xs) ys = if x `elem` ys
                 then x:same xs (delete x ys)
                 else same xs ys


--TASK04
data NTree = Nil | NNode Int [NTree]

t3 :: NTree               --    1
t3 = NNode 1 [NNode 3 [], -- / / \ \
              NNode 5 [], --3 5   7 9
              NNode 7 [],
              NNode 9 []]

                                   --  7
t4 :: NTree                        --  |
t4 = NNode 7 [NNode 9 [NNode 5 [], --  9
                       NNode 2 []]]-- / \
                                   --5   2



--TASK05
t5 :: BTree Int                                                -- 8
t5 = Node 8 (Node 3 (Node 1 Empty Empty)                      -- / \
                    (Node 4 Empty Empty))                    -- 3 10
            (Node 10 (Node 9 Empty Empty)                   -- / \ / \
                    (Node 14 Empty Empty))                  -- 1 4 9 14

t6 :: BTree Int                                                -- 8
t6 = Node 8 (Node 3 (Node 1 Empty Empty)                      -- / \
                    (Node 4 Empty Empty))                     -- 3 10
            (Node 10 (Node 5 Empty Empty)                    -- / \ / \
                    (Node 14 Empty Empty))                  -- 1 4 5 14


t7 :: BTree Int -- 8
t7 = Node 8 (Node 3 (Node 5 Empty Empty) -- / \
                    (Node 6 Empty Empty)) -- 3 10
            (Node 10 (Node 5 Empty Empty) -- / \ / \
                     (Node 14 Empty Empty)) -- 5 6 9 14

isBinarySearchTree :: BTree Int-> Bool
isBinarySearchTree Empty = True
isBinarySearchTree (Node el left right) = helper (<=el) left && helper (>=el) right && isBinarySearchTree left && isBinarySearchTree right
   where
     helper _ Empty = True
     helper current (Node el left right) = current el && helper current left && helper current right





{-

Functions that I used in some of the exercises

--A Function that turns a tree to a list of nodes
treeToList :: BTree a -> [a]
treeToList Empty = []
treeToList (Node x left right) = x : a ++ b
  where a = (treeToList left)
        b = (treeToList right)
    
--A Function that checks if all elements of string1 are in string2
subList :: String-> String -> Bool
subList [] [] = True
subList _ []    = False
subList [] _    = True
subList (x:xs) (y:ys) 
    | x == y    = subList xs ys   
    | otherwise = subList (x:xs) ys

--A Function that ckecks if a node is leaf or not
isLeaf :: Eq a => a -> BTree a -> Bool
isLeaf x = helper
    where helper Empty = False
          helper (Node el Empty Empty) = x == el
          helper (Node _ left right) = helper left || helper right

-}