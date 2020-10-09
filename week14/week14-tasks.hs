import Data.Char(isDigit, digitToInt)
import Data.List(group, nub, sort, sortBy)
import Data.Function(on)

-- От Упражнение 13:
-- Алтернативно решение на Задача 8.
firstWord :: String -> String
firstWord str = takeWhile (/= ' ') $ dropWhile (== ' ') str -- с dropWhile премахваме интервалите в началото, с takeWhile взимаме буквите докато не стигнем до интервал

-- Алтернативно решение на Задача 9 - може да преработим функцията firstWord, така че да връща двойка от първата дума и остатъка от списъка (след първата дума)
firstWordAndRest :: String -> (String, String)
firstWordAndRest str = break (== ' ') $ dropWhile (== ' ') str -- с break разделяме низа според първия интервал - частта преди, и частта след (включва и самия интервал)

words' :: String -> [String]
words' str
    | word == ""        = []
    | all (== ' ') rest = [word]
    | otherwise         = word : words' rest
    where (word, rest) = firstWordAndRest str

-- Задача 11. Да се дефинира функция tighten str, която "сгъстява" низ от думи, като премахва интервалите в началото и в края на низа, 
-- а между всеки две думи оставя по един интервал. Ако низът не съдържа думи, резултатът е празен низ.
tighten :: String -> String
tighten ""             = ""
tighten (' ':cs)       = tighten cs
tighten (c:' ':' ':cs) = tighten (c:' ':cs) -- премахваме повторение на интервали между думи
tighten (c:' ':c':cs)  = c : ' ' : tighten (c':cs) -- тъй като вече сме преминали през горния ред и сме продължили надолу, можем да сме сигурни, 
                                                   -- че c' не е интервал, т.е. е буква. Значи продължаваме "сгъстяването" в остатъка от низа
tighten (c:cs)         = c : tighten cs -- тъй като сме минали през всичките горни редове и сме стигнали до тук, можем да сме сигурни, че сме "вътре" в дума,
                                        -- т.е. c е буква и след него следва още една буква. Значи взимаме първата буква и обработваме остатъка

-- втори вариант - ползваме последните две задачи (всъщност ползваме вградените функции, но с нашите реализации ще е същото, може би по-бавно)
tighten' :: String -> String
tighten' = unwords . words
-- Първо прилагаме words, което ни дава списък само от думите, без никакви интервали. 
-- След това с unwords ги конкатенираме, като добавяме по един интервал между всеки две думи, и така получаваме точно исканият резултат.


-- Задача 12. Да се дефинира функция calcFrequencyTable str, която получава низ str, състоящ се от букви от латинската азбука.
-- Функцията трябва да върне списък от двойки с първи елемент буква (която присъства в str) и втори елемент число (броя на срещанията на буквата в str).
-- Върнатият списък трябва да съдържа всички букви от str, без повторение, и да е сортиран в низходящ ред по броя на срещанията,
-- като при букви с еднакъв брой срещания, първа в списъка трябва да е двойката, чиято буква е с по-малък ASCII код.
calcFrequencyTable :: String -> [(Char, Int)]
calcFrequencyTable "" = []
calcFrequencyTable str = sortBy (\ (_, cnt1) (_, cnt2) -> compare cnt2 cnt1) pairs
    where pairs = [(letter, length (filter (==letter) str)) | letter <- sort (nub str)]
-- Чрез nub получаваме списък от буквите, участващи в str. Чрез sort ги подреждаме по азбучен ред. С list comprehension конструираме списъка от двойките, 
-- като той е подреден само по първите координати на двойките, затова трябва да сортираме и по вторите координати. 
-- Така вече ще получим списък, сортиран по брой срещания, като еднаквите броеве ще са сортирани помежду си по азбучен ред.

-- Второто сортиране извършваме чрез sortBy: вградена функция, която получава функция за сравнение и сортира списъка според нея (обикновеният sort е неин частен случай).
-- compare е метод на класа Ord, който сравнява два елемента и връща някоя от стойностите на класа Ordering - LT, GT, EQ (less than, greater than, equal).
-- Нужно е да ползваме compare, а не просто (<), тъй като sortBy изисква функция връщаща Ordering, а стандартните функции за сравнение връщат Bool.

-- втори вариант
calcFrequencyTable' "" = []
calcFrequencyTable' cs = sortBy (\ (_, cnt1) (_, cnt2) -> compare cnt2 cnt1) pairs
    where pairs = [(head x, length x) | x <- group (sort cs)]
-- Тук не ползваме nub, а директно сортираме низа. С group получаваме списък от списъци от последователни еднакви елементи.
-- От него може да получим списъка от двойки, който пак ще е сортиран само по първите координати, затова сортираме по вторите координати чрез sortBy.

-- трети вариант - сортиране без нужда от ламбда
calcFrequencyTable'' "" = []
calcFrequencyTable'' cs = sortBy (flip compare `on` snd) pairs
    where pairs = [(head x, length x) | x <- group (sort cs)]
-- Като комбинираме compare със специалната функция on, може директно да сравняваме по вторите координати на двойките
-- Така обаче ще получим възходящ ред, затова ползваме flip, която просто обръща аргументите на сравнението, за да получим низходящ ред


-- Задача 13. Run-length encoding е прост начин за компресия на текст, при който последователните срещания на един елемент (символ от текста) се заменят с 
-- <брой на срещания><елемент>.
-- В случай, че в резултат на тази замяна биха се получили поне толкова символи, колкото се съдържат в оригиналния текст, тя не се извършва. Например, 
-- ако имаме само едно срещане на буквата "а" и го заменим с "1а", то промененият текст ще има повече символи от оригиналния, затова текстът остава непроменен. 
-- Да се дефинира функция encode, която компресира низ по описания метод. Да се дефинира и обратната функция decode.
encode :: String -> String
encode cs = concat [compress ss | ss <- group cs] -- или в безаргументен стил: encode = concat . map compress . group
    where
        compress ss@[_]   = ss -- еднобуквени низове няма нужда да се компресират
        compress ss@[_,_] = ss -- двубуквени низове няма нужда да се компресират
        compress ss@(s:_) = show (length ss) ++ [s] -- length ни връща число, което чрез show превръщаме в низ от цифрите му

decode :: String -> String
decode cs = helper cs 0
    where
        helper ""     _ = ""
        helper (c:cs) k
            | isDigit c = helper cs (k * 10 + digitToInt c)
            | k == 0    = c : helper cs 0
            | otherwise = replicate k c ++ helper cs 0



main :: IO()
main = do
    --print $ firstWord "This is a sentence"       -- -> "This"
    --print $ firstWord "   This   is a sentence"  -- -> "This"
    --print $ firstWordAndRest "   This   is a sentence" -- -> ("This","   is a sentence")
    --print $ words' "  This   is  a   sentence    "     -- -> ["This","is","a","sentence"]

    --print $ tighten "  This   is  a   sentence    "   -- -> "This is a sentence"
    --print $ tighten' "  This   is  a   sentence    "  -- -> "This is a sentence"


    --print $ sort (nub "ababacccc") -- -> "abc"
    --print [(letter, length (filter (==letter) "ababacccc")) | letter <- "abc"] -- -> [('a',3),('b',2),('c',4)]

    --print $ group (sort "ababacccc") -- -> ["aaa","bb","cccc"]
    --print [(head x, length x) | x <- ["aaa","bb","cccc"]] -- -> [('a',3),('b',2),('c',4)]

    --print $ calcFrequencyTable "ababac"    -- -> [('a',3),('b',2),('c',1)]
    --print $ calcFrequencyTable "aaabbbc"   -- -> [('a',3),('b',3),('c',1)]
    --print $ calcFrequencyTable "ababacccc" -- -> [('c',4),('a',3),('b',2)]
    --print $ calcFrequencyTable "bababa"    -- -> [('a',3),('b',3)]


    --print $ encode "a"                -- -> "a"
    --print $ encode "Haskell"          -- -> "Haskell"
    --print $ encode "aaabccdefff"      -- -> "3abccde3f"
    --print $ encode "aaaaaaaaaaaabbb"  -- -> "12a3b"

    --print $ decode "a"                -- -> "a"
    --print $ decode "Haskell"          -- -> "Haskell"
    --print $ decode "3abccde3f"        -- -> "aaabccdefff"
    --print $ decode "12a3b"            -- -> "aaaaaaaaaaaabbb"

    --print $ encode "aaaaaaaaaaaaaaaaaaaaaaaaaaaaabccdddeeee" -- -> "29abcc3d4e"
    --print $ decode "29abcc3d4e" -- -> "aaaaaaaaaaaaaaaaaaaaaaaaaaaaabccdddeeee"
