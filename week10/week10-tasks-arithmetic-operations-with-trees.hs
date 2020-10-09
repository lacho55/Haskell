-- Представяне на аритметични изрази чрез двоични дървета:
-- по листата стоят числа, а по вътрешните върхове - инфиксни операции (+, -, *, /, ^)

-- За да бъде по-ясно кои върхове са листа (и съответно трябва да не съдържат операции, а стойности), може да дефинираме типа двоично дърво така:
data BTree a = Empty | Leaf a | Node a (BTree a) (BTree a) deriving Show
                                                    
expr1 :: BTree Char                                 --      -
expr1 = Node '-' (Node '+' (Leaf '5')               --     / \
                           (Node '*' (Leaf '2')     --    +   4
                                     (Leaf '5')))   --   / \
                 (Leaf '4')                         --  5   *
                                                    --     / \
                                                    --    2   5
getExpression :: BTree Char -> String
getExpression Empty                = []
getExpression (Leaf c)             = [c]
getExpression (Node c left right)  = "(" ++ (getExpression left) ++ [c] ++ (getExpression right) ++ ")"

-- Така може да отпечатаме израза, представен чрез дървото. Как може да изчислим стойността му?

-- 1ви вариант - по-специален вид дърво, с възможност за различен тип стойности по вътрешните възли и листата
data ExprTree node leaf = EmptyExpr |
                          ExprLeaf leaf | 
                          ExprNode node (ExprTree node leaf) (ExprTree node leaf) deriving Show
-- това дърво вече има два типа - node и leaf (можеха да бъдат просто a и b, но типовите променливи могат да имат и по-дълги и описателни имена)
-- възлите имат стойност от първия тип, а листата от втория

expr2 :: ExprTree Char Double
expr2 = ExprNode '-' (ExprNode '+' (ExprLeaf 5)
                                   (ExprNode '*' (ExprLeaf 2) 
                                                 (ExprLeaf 5)))
                     (ExprLeaf 4)

calc :: ExprTree Char Double -> Double
calc EmptyExpr    = 0
calc (ExprLeaf x) = x -- при листо няма какво да смятаме - трябва просто да върнем стойността, която стои в самото листо
calc (ExprNode op left right)
    | op == '+' = calc left + calc right
    | op == '-' = calc left - calc right
    | op == '*' = calc left * calc right
    | op == '/' = calc left / calc right
    | op == '^' = calc left ** calc right
    | otherwise = error "Invalid tree"

-- 2ри вариант - дърво от низове и ползване на функцията read
expr3 :: BTree String
expr3 = Node "-" (Node "+" (Leaf "5")
                           (Node "*" (Leaf "2") 
                                     (Leaf "5")))
                 (Leaf "4")

calc' :: BTree String -> Double
calc' Empty    = 0
calc' (Leaf x) = read x 
calc' (Node op left right)
    | op == "+" = calc' left + calc' right
    | op == "-" = calc' left - calc' right
    | op == "*" = calc' left * calc' right
    | op == "/" = calc' left / calc' right
    | op == "^" = calc' left ** calc' right
    | otherwise = error "Invalid tree"
-- read е нещо като обратната функция на show - докато show взима стойност и я конвертира в низ (който после може да отпечатаме на конзолата чрез print), 
-- read взима низ и го конвертира до съответната стойност. Тук във втория случай на функцията calc' ползваме read - благодарение на сигнатурата на функцията 
-- компилатора знае че трябва да се върне Double, и така преобразува низа в стойност от тип Double.

main :: IO()
main = do
    print $ getExpression expr1 -- -> "((5+(2*5))-4)"

    print $ calc expr2  -- -> 11.0

    print $ calc' (Leaf "1")          -- -> 1.0
    print $ calc' (Leaf "123.456789") -- -> 123.456789
    print $ calc' expr3               -- -> 11.0


