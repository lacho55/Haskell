import Data.Char(ord, isDigit, isUpper, toUpper)
import Data.List(isPrefixOf, group)

-- Упражнение 13: Низове
-- String - списък от Char-ове (type String = [Char])
-- Със низове се работи по същия начин както се работи със списъци:
-- можем да ги обхождаме с рекурсия, да ги map-ваме и филтрираме, да ги генерираме с list comprehension и т.н.

-- Задача 1. Да се дефинира функция digits str, която получава низ str и връща низ, съдържащ само цифрите от str.
digits :: String -> String
digits str = [ch | ch <- str, isDigit ch] -- isDigit е вградена функция от библиотеката Data.Char, която проверява дали символ е цифра

-- Задача 2. Да се дефинира функция digitsSum str, която изчислява сумата на цифрите в даден низ.
digitsSum :: String -> Int
digitsSum "" = 0 -- "" тук е празен низ, което е еквивалентно на празен списък. Може да ползваме и [], но с "" става по-ясно че работим конкретно със низове
digitsSum (c:cs) -- c:cs идва от char:chars. Може да пишем и x:xs както правим обикновено, но както с "", така показваме на четящия кода че става дума за символи и низове
    | isDigit c = ord c - ord '0' + digitsSum cs
    | otherwise = digitsSum cs
-- ord връща ASCII кода на цифрата, но за да получим самата цифра изваждаме ASCII кода на 0. Например за 5: ord '5' - ord '0' = 53 - 48 = 5

-- второ решение, ползващо миналата задача
digitsSum' :: String -> Int
digitsSum' cs = sum [ord c - ord '0' | c <- digits cs]

-- Задача 3. Да се дефинира функция capitalize str, която прави всички малки букви в даден низ главни.
capitalize :: String -> String
capitalize cs = map toUpper cs -- toUpper е вградена функция от библиотеката Data.Char, която конвертира малка буква към главна 
                               -- (а други символи, като главни букви, цифри, и т.н. оставя непроменени)

-- Задача 4. Да се дефинира предикат isCapitalized str, която проверява дали всички букви в даден низ са главни.
isCapitalized :: String -> Bool
isCapitalized ""     = True
isCapitalized (c:cs) = isUpper c && isCapitalized cs

isCapitalized' :: String -> Bool
isCapitalized' cs = all isUpper cs

isCapitalized'' :: String -> Bool
isCapitalized'' cs = cs == capitalize cs

-- Задача 5. Да се дефинира функция nOrMoreVowels words n, която получава списък от думи words и число n и връща списък от тези думи в words, които имат поне n гласни.
isVowel :: Char -> Bool
isVowel c = elem c "aeiouy"

countVowels :: String -> Int
--countVowels word = length (filter isVowel word)    -- 1ви вариант
countVowels word = length [c | c <- word, isVowel c] -- 2ри вариант

nOrMoreVowels :: [String] -> Int -> [String]
--nOrMoreVowels words n = filter (\word -> countVowels word >= n) words -- 1ви вариант
nOrMoreVowels words n = [word | word <- words, countVowels word >= n]   -- 2ри вариант

-- Задача 6. Да се дефинира предикат isInfixOf xs ys, която получава два низа и проверява дали първия е инфикс на втория.
-- 1ви вариант
isInfixOf :: String -> String -> Bool
isInfixOf [] _     = True
isInfixOf (_:_) [] = False
isInfixOf xs ys    = isPrefixOf xs ys || isInfixOf xs (tail ys)
-- isPrefixOf е вградена функция от библиотеката Data.List, която проверява дали списък е префикс на друг списък,
-- подобно на функцията prefix която написахме в Упражнение 5, Задача 4

-- 2ри вариант
-- първо - функция, която генерира всички опашки (т.е. суфикси) на даден списък
tails :: [a] -> [[a]]
tails []        = [[]] -- ако върнем [] то празният списък няма да участва като опашка 
tails ys@(_:xs) = ys : tails xs
-- второ - проверяваме дали първият низ е префикс на някоя от опашките на втория низ. 
-- Това дефакто правим и в първия вариант, но там обхождаме опашките една по една, а тук ползваме вградената функция any
isInfixOf' :: String -> String -> Bool
isInfixOf' xs ys = any (isPrefixOf xs) (tails ys)

-- Забележка: isPrefixOf, isInfixOf и tails всъщност са вградени функции от библиотеката Data.List (заедно с inits, която ползвахме в Упражнение 5, Задача 4)
-- Библиотечната реализация на isInfixOf е подобна на втория вариант тук.

-- Задача 7. Да се дефинира функция longestSubstring str, която получава низ str и намира дължината на най-дългия му подниз, състоящ се от еднакви символи.
longestSubstring :: String -> Int
longestSubstring str = maximum [length substring | substring <- group str]
-- group е вградена функция (от Data.List), която разделя списък на списъци от последователни еднакви елементи.
-- Например: group "Mississippi" -> ["M","i","ss","i","ss","i","pp","i"]


-- Задача 8. Да се дефинира функция firstWord str, която получава низ и извлича първата "дума" (подниз без интервали) от него. 
-- Връща празен низ, ако аргументът не съдържа дума.
firstWord :: String -> String
firstWord ""         = ""
firstWord (' ':cs)   = firstWord cs -- прескачаме интервали в началото
firstWord (c:' ':cs) = [c] -- стигнали сме до края на първата дума, значи трябва да завършим. Връшаме списък от буквата, а не самата буква, 
                           -- тъй като вторият аргумент на (:) е списък
firstWord (c:c':cs)  = c : firstWord (c':cs) -- тъй като сме минали през горния ред и сме продължили надолу, значи можем да сме сигурни че c' не е интервал,
                                             -- т.е. е буква и значи в момента сме в първата дума. Взимаме първата ѝ буква и обработваме остатъка
firstWord cs         = cs

-- втори вариант - с вградените функции dropWhile и takeWhile
firstWord' :: String -> String
firstWord' str = takeWhile (/= ' ') $ dropWhile (== ' ') str -- с dropWhile премахваме интервалите в началото, с takeWhile взимаме буквите докато не стигнем до интервал

-- Задача 9. Да се дефинира функция words' str, която получава низ и образува списък от "думите" в него (това са поднизове, разделени с един или повече интервала).
-- Името е с ' накрая, тъй като има вградена функция (от Prelude) с това име.
words' :: String -> [String]
words' ""         = [] -- вече имаме не низ, а списък от низове, т.е. трябва да ползваме стандартния празен списък
words' (' ':cs)   = words' cs -- прескачаме интервали
words' [c]        = [[c]] -- имаме дума от една буква
words' (c:' ':cs) = [c] : words' cs -- стигнали сме до края на дума
words' (c:c':cs)  = (c:rs):rss -- знаем че сме в дума, значи взимаме първата ѝ буква и обработваме остатъка. Резултатът от тази обработка е списък от думи.
    where rs:rss = words' (c':cs) --  Добавяме първата буква към първата дума на този списък и получената нова дума добавяме към остатъка от списъка.
-- реализацията на вградената функция words е доста по-различна, тя не обхожда низа символ по символ. Най-вероятно е и по-ефективна.

-- втори вариант - може да преработим функцията firstWord, така че да връща двойка от първата дума и остатъка от списъка (след първата дума)
firstWordAndRest :: String -> (String, String)
firstWordAndRest str = break (== ' ') $ dropWhile (== ' ') str -- с break разделяме низа според първия интервал - частта преди, и частта след (включва и самия интервал)

words'' :: String -> [String]
words'' str
    | word == ""        = []
    | all (== ' ') rest = [word]
    | otherwise         = word : words'' rest
    where (word, rest) = firstWordAndRest str

-- Задача 10. Да се дефинира функция unwords' ws, която получава списък от низове ws и ги конкатенира в един цял низ,
-- разделяйки отделните думи с по един интервал. Името е с ' накрая, тъй като има вградена функция (от Prelude) с това име.
unwords' :: [String] -> String
unwords' [] = ""
unwords' ws = foldr1 (\w s -> w ++ ' ':s) ws


main :: IO()
main = do
    --print $ digits "d321as231dSdadaSA"     -- -> "321231"
    --print $ digitsSum "d321as231dSdadaSA"  -- -> 12
    --print $ digitsSum' "d321as231dSdadaSA" -- -> 12
 
    --print $ capitalize "Abcd"    -- -> "ABCD"
    --print $ isCapitalized "ABCD" -- -> True
    --print $ isCapitalized "AbCD" -- -> False
 
    --print $ nOrMoreVowels ["cat", "dog", "doggo", "kitten", "rat"] 2 -- -> ["doggo", "kitten"]

    --print $ tails [1,2,3,4] -- -> [[1,2,3,4],[2,3,4],[3,4],[4],[]]
    --print $ isInfixOf "can" "I can't" -- -> True
    --print $ isInfixOf "can't" "I can" -- -> False

    --print $ group "111228888236555"                                 -- -> ["111","22","8888","2","3","6","555"]
    --print [length substring | substring <- group "111228888236555"] -- -> [3,2,4,1,1,1,3]
    --print $ longestSubstring  "111228888236555"                     -- -> 4 (заради 8888)


    --print $ firstWord "This is a sentence"       -- -> "This"
    --print $ firstWord "   This   is a sentence"  -- -> "This"
    --print $ firstWord' "This is a sentence"      -- -> "This"
    --print $ firstWord' "   This   is a sentence" -- -> "This"

    --print $ words' "  This   is  a   sentence    "     -- -> ["This","is","a","sentence"]
    --print $ firstWordAndRest "   This   is a sentence" -- -> ("This","   is a sentence")
    --print $ words'' "  This   is  a   sentence    "    -- -> ["This","is","a","sentence"]

    --print $ unwords ["This","is","a","sentence"]     -- -> "This is a sentence"