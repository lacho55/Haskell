import Data.Char(isLetter, isDigit, toUpper)
import Data.List(isPrefixOf, tails, elemIndex)
import Data.Maybe(fromJust)
 
-- Задача 1. Нормализация на входните данни
-- Енигма, както повечето криптиращи машини от това време, е разполагала с клавиатура със само 26-те главни букви от латинската азбука. Затова, преди да бъдат 
-- криптирани, всички съобщения трябвало да бъдат приведени в т. нар. нормален вид: всички числени стойности бивали изписвани словом, всички малки букви ставали 
-- главни, а интервалите и пунктуационните знакове били премахвани или заменяни с кодови комбинации от главни букви (напр. интервалът бил заменян с X и т. н.).

-- Напишете функция normalize message, която нормализира входното съобщение. Правилата за нормализация са следните: 
-- - всички малки букви стават главни
-- - ако съобщението съдържа цифри, функцията връща грешка
-- - всички останали знакове се игнорират

normalize :: String -> String
normalize [] = []
normalize (x:xs)
    | isDigit x  = error "digits not allowed"
    | isLetter x = toUpper x : normalize xs
    | otherwise  = normalize xs

normalize' :: String -> String
normalize' msg = if any isDigit msg then error "digits not allowed" else map toUpper $ filter isLetter msg

-- Задача 2. Цезаров шифър
-- Цезаровият шифър е един от най-простите и най-стари методи за криптиране на съобщения. Първото му известно използване е от Юлий Цезар по време на кампаниите му в 
-- Галия, откъдето идва и неговото име. Идеята на Цезаровия шифър е проста: вземаме съобщението, което искаме да шифроваме, и заместваме всяка от буквите в него с 
-- буквата, отместена с определен брой позиции в азбуката. Например, ако отместването е 3, то тогава 
-- ‘A’ -> ‘D’, ‘B’ -> ‘E’, ‘C’ -> ‘F,’ ... , ‘X’ -> ‘A’, ‘Y’ -> ‘B’, ‘Z’ -> ‘C’.

-- а) Напишете функция encode alphabet ch offset, която приема списък от знакове alphabet, знак ch и отместване offset и връща знака от alphabet, 
-- отместен на offset от ch (по модул дължината на списъкa). Функцията encode трябва да работи както с положително, така и с отрицателно отместване 
-- и да връща грешка, ако ch не е елемент на alphabet.
-- N.B. Не е задължително буквите в alphabet да са подредени от ‘A’ до ‘Z’, т.е. НЕ може да разчитате на функциите ord и chr!
encode :: String -> Char -> Int -> Char
encode alphabet ch offset = if notElem ch alphabet then error ("unsupported symbol: " ++ [ch]) else alphabet!!position
    where position = (fromJust (elemIndex ch alphabet) + offset) `mod` (length alphabet)
    -- elemIndex ни връша Nothing ако елемента не присъства в списъка в който търсим, или Maybe индекса ако присъства. Ние обаче викаме position само за такива букви,
    -- които вече сме проверили че са елементи (тъй като не са изпълнили notElem, not(notElem) === elem), 
    -- така че може спокойно да извикаме fromJust (който работи само за Maybe стойности)
 
-- б) Напишете функция encrypt alphabet offset normalized, която приема азбука alphabet, отместване offset и съобщение в нормализиран вид и връща съобщението, 
-- криптирано със съответното отместване.
encrypt :: String -> Int -> String -> String
encrypt alphabet offset normalized = [encode alphabet letter offset | letter <- normalized]

-- в) Напишете функция decrypt alphabet offset encrypted, която приема отместване offset и съобщение, криптирано с това отместване, и връща оригиналното съобщение 
-- в нормализиран вид. Можете да използвате факта, че декриптирането на Цезаров шифър с дадено отместване offset е еквивалентно на криптиране с отместване -offset.
decrypt :: String -> Int -> String -> String
decrypt alphabet offset encrypted = encrypt alphabet (-offset) encrypted

-- Задача 3. Атака на Цезаровия шифър
-- Една от основните слабости на Цезаровия шифър се състои в това, че броят на възможните шифри е ограничен до броя на ротациите на буквите в азбуката минус едно. 
-- Това прави Цезаровия шифър податлив на т. нар. brute force атака, т.е. атака, която генерира всички възможни дешифровки на кодираното съобщение.

-- а) Напишете функцията crackAll alphabet encrypted, която връща списък от всички възможни дешифровки на кодираното съобщение encrypted.
crackAll :: String -> String -> [String]
crackAll alphabet encrypted = [decrypt alphabet offset encrypted | offset <- [1 .. (length alphabet - 1)]]
 
-- б) След като сме генерирали всички възможни дешифровки, бихме могли лесно да намерим най-вероятните от тях, използвайки факта, че някои кратки думи, например 
-- the, at, on, се срещат много често в английския език. За тази цел най-напред напишете функция substring sub str, която проверява дали поднизът sub се среща в низа str.
substring :: String -> String -> Bool
substring sub str = any (isPrefixOf sub) (tails str)

-- в) Използвайте функциите от предишните две подточки, за да напишете функцията crackCandidates alphabet commonWords encrypted, която приема списък с 
-- често срещани думи и криптирано съобщение и връща списък с потенциални вероятни разшифровки. Една разшифровка се смята за вероятна, ако съдържа поне 
-- една от думите от списъка с често срещани думи.
crackCandidates :: String -> [String] -> String -> [String]
crackCandidates alphabet commonWords encrypted = [x | x <- crackAll alphabet encrypted, isCandidate x]
    where isCandidate str = any (`substring` str) commonWords
 
-- Задача 4. Polysubstitution cypher (шифър с множествено заместване) 
-- Един от простите начини да се справим със слабостта на Цезаровия шифър е да разбием съобщението на блокове от по няколко знака и да криптираме всеки от тях 
-- с различен Цезаров шифър, отместен с определена стъпка спрямо предишния.

-- а) Напишете функция polyEncrypt alphabet offset step blockSize normalized, която приема азбука alphabet, първоначално отместване offset, стъпка step 
-- и размер на блока blockSize, както и съобщение в нормализиран вид normalized, и връща криптирано съобщение, първите blockSize знака на което се криптират 
-- с отместване offset, следващите blockSize знака – с отместване offset+step и т. н. 
polyEncrypt :: String -> Int -> Int -> Int -> String -> String
polyEncrypt _ _ _ _ ""                                = ""
polyEncrypt alphabet offset step blockSize normalized = encrypt alphabet offset (take blockSize normalized) ++ 
                                                        polyEncrypt alphabet (offset + step) step blockSize (drop blockSize normalized)

-- б) Напишете функция polyDecrypt alphabet offset step blockSize encrypted, която декриптира съобщението от предишната подточка. 
polyDecrypt :: String -> Int -> Int -> Int -> String -> String
polyDecrypt alphabet offset step blockSize encrypted = polyEncrypt alphabet (-offset) (-step) blockSize encrypted

-- Задача 5. Емулация на Eнигма 
-- Един от основните компоненти на Енигма е система от ротори, всеки от които може да се моделира като polysubstitution cypher от предната задача. 
-- Резултатът от всеки от роторите се предава като вход на следващия. Резултатът от последния ротор е криптираното съобщение.

-- а) Напишете функция enigmaEncrypt alphabet rotors normalized, която приема азбука alphabet, списък от ротори (offset, step, blockSize) 
-- и съобщение в нормализиран вид normalized и връща криптираното от роторите съобщение. 
enigmaEncrypt :: String -> [(Int,Int,Int)] -> String -> String
enigmaEncrypt _        []                               message    = message
enigmaEncrypt alphabet ((offset,step,blockSize):rotors) normalized = enigmaEncrypt alphabet rotors (polyEncrypt alphabet offset step blockSize normalized)

-- б) Напишете функция enigmaDecrypt alphabet rotors normalized, която приема азбука, списък от ротори и криптирано съобщение и връща оригиналното съобщение. 
enigmaDecrypt :: String -> [(Int,Int,Int)] -> String -> String
enigmaDecrypt _        []                               message    = message
enigmaDecrypt alphabet ((offset,step,blockSize):rotors) normalized = enigmaDecrypt alphabet rotors (polyDecrypt alphabet offset step blockSize normalized)

main :: IO()
main = do
    --print $ normalize "Attack London tommorow at ten a.m." -- -> "ATTACKLONDONTOMMOROWATTENAM"
    --print $ normalize "Attack London tommorow at 10 a.m."  -- error: ... digits not allowed
    --print $ normalize' "Attack London tommorow at ten a.m." -- -> "ATTACKLONDONTOMMOROWATTENAM"
    --print $ normalize' "Attack London tommorow at 10 a.m."  -- error: ... digits not allowed

    --print $ encode ['A'..'Z'] 'A' 1     -- -> 'B'
    --print $ encode ['A'..'Z'] 'C' 2     -- -> 'E'
    --print $ encode ['A'..'Z'] 'Z' 3     -- -> 'C'
    --print $ encode ['A'..'Z'] 'A' (-1)  -- -> 'Z'
    --print $ encode ['A'..'Z'] 'C' (-2)  -- -> 'A'
    --print $ encode ['A'..'Z'] 'Z' (-3)  -- -> 'W'
    --print $ encode ['A'..'Z'] '@' 1     -- error: ... unsupported symbol: @
    --print $ encode ['A'..'Z'] 'a' 1     -- error: ... unsupported symbol: a
    --print $ encode ['A','C'..'Z'] 'C' 3 -- -> 'I' (азбуката ни всъщност е A,C,E,G,I,..., така че 3 символа след C е I)
 
    --print $ encrypt ['A'..'Z'] 5 "ATTACKLONDONTOMORROWATTENAM" -- -> "FYYFHPQTSITSYTRTWWTBFYYJSFR"
    --print $ decrypt ['A'..'Z'] 5 "FYYFHPQTSITSYTRTWWTBFYYJSFR" -- -> "ATTACKLONDONTOMORROWATTENAM"

    --print $ crackAll ['A'..'Z'] "FYYFHPQTSITSYTRTWWTBFYYJSFR" 
    {- -> ["EXXEGOPSRHSRXSQSVVSAEXXIREQ","DWWDFNORQGRQWRPRUURZDWWHQDP","CVVCEMNQPFQPVQOQTTQYCVVGPCO","BUUBDLMPOEPOUPNPSSPXBUUFOBN","ATTACKLONDONTOMORROWATTENAM",
    "ZSSZBJKNMCNMSNLNQQNVZSSDMZL","YRRYAIJMLBMLRMKMPPMUYRRCLYK","XQQXZHILKALKQLJLOOLTXQQBKXJ","WPPWYGHKJZKJPKIKNNKSWPPAJWI","VOOVXFGJIYJIOJHJMMJRVOOZIVH",
    "UNNUWEFIHXIHNIGILLIQUNNYHUG","TMMTVDEHGWHGMHFHKKHPTMMXGTF","SLLSUCDGFVGFLGEGJJGOSLLWFSE","RKKRTBCFEUFEKFDFIIFNRKKVERD","QJJQSABEDTEDJECEHHEMQJJUDQC",
    "PIIPRZADCSDCIDBDGGDLPIITCPB","OHHOQYZCBRCBHCACFFCKOHHSBOA","NGGNPXYBAQBAGBZBEEBJNGGRANZ","MFFMOWXAZPAZFAYADDAIMFFQZMY","LEELNVWZYOZYEZXZCCZHLEEPYLX",
    "KDDKMUVYXNYXDYWYBBYGKDDOXKW","JCCJLTUXWMXWCXVXAAXFJCCNWJV","IBBIKSTWVLWVBWUWZZWEIBBMVIU","HAAHJRSVUKVUAVTVYYVDHAALUHT","GZZGIQRUTJUTZUSUXXUCGZZKTGS"] -}
    --print $ substring "Haskell" "Haskell Curry" -- -> True
    --print $ substring "Curry" "Haskell Curry"   -- -> True
    --print $ substring "Turing" "Haskell Curry"  -- -> False
    --print $ crackCandidates ['A'..'Z'] ["THE","AND","AT","ON","IS"] "FYYFHPQTSITSYTRTWWTBFYYJSFR" -- -> ["ATTACKLONDONTOMORROWATTENAM"]
 
    --print $ polyEncrypt ['A'..'Z'] 5 1 7 "ATTACKLONDONTOMORROWATTENAM" -- -> "FYYFHPQUTJUTZUTVYYVDHBBMVIU"
    --print $ polyDecrypt ['A'..'Z'] 5 1 7 "FYYFHPQUTJUTZUTVYYVDHBBMVIU" -- -> "ATTACKLONDONTOMORROWATTENAM"
    --print $ enigmaEncrypt ['A'..'Z'] [(5,1,1),(7,2,10),(13,3,25)] "ATTACKLONDONTOMORROWATTENAM" -- -> "ZTUCFOQUULZZGCBEIJHQXRSEOFS"
    --print $ enigmaDecrypt ['A'..'Z'] [(5,1,1),(7,2,10),(13,3,25)] "ZTUCFOQUULZZGCBEIJHQXRSEOFS" -- -> "ATTACKLONDONTOMORROWATTENAM"
