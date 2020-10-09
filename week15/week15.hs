--  Упражнение 15: Безкрайни списъци
-- iterate многократно прилага функция, създавайки списък от резултатите [x, f x, f (f x), f (f (f x)), ...]
iterate' :: (a -> a) -> a -> [a]
iterate' f x = x : iterate' f (f x)
-- тук нямаме дъно на рекурсията, тоест резултатният списък ще е безкраен

-- Как например да получим списък от всички естествени числа?
-- Като започнем от нулата и многократно добавяме единица.
nats = iterate (+1) 0
-- Това е безкраен списък [0, 1, 2, 3, 4, 5,...]. Ако например се опитаме да му вземем length, ще се получи бездънна рекурсия, 
-- тъй като length никога няма да стигне до празен списък, т.е. няма да стигне до дъното на рекурсията и следователно няма да приключи изпълнението си.

-- Но Haskell използва lazy evaluation - оценява само когато е нужно. Така ако искаме да вземем първите 10 елемента на списъка, то може да използваме take без проблем.
-- Ще бъде оценена само началната част на списъка до 10тия елемент. Останалите (безкрайно много) елементи все още не са оценени, и следователно не създават проблем.

-- Може да генерираме безкрайни списъци и с list comprehension.
nats' = [0..]


-- Как да генерираме списък от всички прости числа?
-- Първо ни трябва предикат, който проверява дали число е просто.
divides :: Integer -> Integer -> Bool
divides x y = y `rem` x == 0

divisors :: Integer -> [Integer]
divisors x = [y | y <- [1..x], y `divides` x]

isPrime :: Integer -> Bool 
isPrime x = [1, x] == divisors x -- число е просто, ако единствените му делители са 1 и самото число

-- Сега като знаем как да проверим дали дадено число е простo и имаме всичките (естествени) числа, може да направим:
primes :: [Integer]
primes = [p | p <- tail nats, isPrime p] -- взимаме tail, за да прескочим нулата
-- -> [2, 3, 5, 7, 11,...]

-- Ефективен (но не най-ефективния) начин за генериране на прости числа е решето на Ератостен: https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes
sieve :: [Integer] -> [Integer]
sieve []     = []
sieve (x:xs) = x : sieve [y | y <- xs, y `mod` x > 0]

primes' :: [Integer]
primes' = sieve [2..]


-- Как да генерираме списък от всичките факториели? ([1!, 2!, 3!, ...])
-- Първо може просто да map-нем списъка nats.
facts :: [Integer]
facts = map (\ n -> product [1..n]) [1..] -- тук пропускаме нулата
-- Но това ще е доста бавно - за всяко число ще генерираме списъка от всички числа между 1 и самото него и ще ги умножаваме.
-- Имаме че n! = n * (n-1)!, но как може да го използваме?
-- Нека умножим всичките ествествени числа: 1*2*3*4*... - това може да получим с foldl (*) [1..] 
-- (дефакто няма да получим краен резултат - няма как да умножим безкрайно много числа).

-- Сега от това бекрайно умножение нека "изрежем" сегмента от 1 до n. Ще получим 1*2*3*...*(n-1)*n, което е точно n!.
-- Тоест искаме да fold-нем, но не само да сметнем финален резултат, а да имаме и междинни стойности. За тази цел има вградени функции scanr и scanl,
-- които са доста подобни като код на съответните им fold-ове.
foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ v []     = v
foldl' f v (x:xs) = foldl' f (v `f` x) xs

scanl' :: (b -> a -> b) -> b -> [a] -> [b] -- връщаме списък, а не само един краен резултат
scanl' _ v []     = [v]
scanl' f v (x:xs) = v : scanl' f (v `f` x) xs -- освен рекурсивното извикване имаме и добавяме към списъка
-- Предимство на foldl е, че аргументът, служещ за начална стойност всъщност след първата стъпка се ползва точно за съхраняване на междинна стойност.

facts' :: [Integer]
facts' = scanl (*) 1 [2..] -- ако списъкът е [1..] се получава една излишна единица в началото

facts'' = scanl1 (*) [1..] -- знаем че списъкът е непразен, и както при fold-овете има варианти на scan за непразни списъци които не изискват начална стойност

-- Как да генерираме списък от всички числа на Фибоначи?
-- Нека първо си припомним как генерираме конкретно n-то число на Фибоначи.
fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

-- Първо може да пробваме да ги получим от списъка nats, който вече имаме,
fibs :: [Integer]
fibs = map fib nats
-- но както при факториелите, това е доста бавно - за всяко число n ще смятаме от нулата съответното му n-то число на Фибоначи,
-- без да ползваме, че има връзка между следващото и предишното.

-- Имаме, че първите 2 числа на Фибоначи са 1 и 1, и всяко следващо се получава от миналите 2
-- Нека имаме списък на числата на Фибоначи до например 6тото. Сега да вземем още едно копие на този списък и да го "отместим" наляво
--   [1,1,2,3,5,8]
-- [1,1,2,3,5,8]
-- и сега ги сумираме поелементно, като игнорираме частите най-отляво и най-отдясно (които нямат съответен елемент от другия списък)
--   [2,3,5,8,13]
-- Накрая на резултатния списък получихме следващото число на Фибоначи. Ако приложим същата идея отново ще получим 21 като 13 + 8, и ако продължаваме 
-- на всяка стъпка ще получаваме по още едно число на Фибоначи.

-- Как може да реализираме това поелементно събиране? Първо за да извършваме операции върху двойки елементи на 2 списъка ползваме zipWith.
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ []     _      = []
zipWith' _ _      []     = []
zipWith' f (a:as) (b:bs) = f a b : zipWith' f as bs

fibs' = 1 : 1 : zipWith (+) fibs' (tail fibs')
-- Започваме със списък от две единици. С tail fibs' всъщност симулираме "отместването", споменато по-горе - тук просто махаме първия елемент.
-- [1,1,2,3,5,8]
-- [1,2,3,5,8]
-- zipWith (+)
-- [2,3,5,8,13]
-- С всяко рекурсивно извикване ще генерираме по още едно число, и така до безкрайност.


-- Пирагорова тройка е тройка от числа, които могат да бъдат страни на правоъгълен триъгълник, тоест удовлетворяват тъждеството от Питагоровата теорема.
pythagTriples :: [(Int,Int,Int)]
pythagTriples = [(x, y, z) | x <- [2..], y <- [x+1..], z <- [y+1..], x*x + y*y == z*z]
-- това няма да работи, тъй като имаме няколко безкрайни генератора - ще изберем стойностите x=2, y=2+1=3, z=y+1=4, които не удовлетворяват равенството,
-- следователно ще изберем следваща стойност на z, и следваща, и следваща, и т.н. до безкрайност - когато имаме няколко безкрайни генератора, 
-- то последният от тях се върти до безкрайност и другите не успяват да направят нищо.

pythagTriples' :: [(Int,Int,Int)]
pythagTriples' = [(x, y, z) | z <- [2.. ], y <- [2..z-1], x <- [2..y-1], x*x + y*y == z*z]
-- така вече ще работи, тъй като имаме само един безкраен генератор - този за z. Стойностите на x и y са ограничени от z, и така техните генератори са крайни.

-- member е фунцкия, която проверява дали даден елемент принадлежи на списък
member :: Eq a => a -> [a] -> Bool
member _ []     = False
member x (y:ys) = if x == y then True else member x ys
-- но ако списъка е безкраен и елемента не принадлежи, то функцията никога няма да завърши, т.к. никога няма да стигне до []

-- Ако безкрайният списък е сортиран, то може да търсим в него без да имаме проблем ако елемента не присъства
memberOrd :: Ord a => a -> [a] -> Bool
memberOrd x (y:ys)
    | x > y     = memberOrd x ys
    | x == y    = True
    | otherwise = False -- елементът x, който търсим, е < от текущия елемент на списъка, но списъкът е сортиран, така че x ще е < и от всички следващи елементи, и
                        -- следователно няма да присъства в списъка


main :: IO()
main = do
    --print $ take 10 nats   -- -> [0,1,2,3,4,5,6,7,8,9]
    --print $ take 10 nats'
 
    --print $ take 10 primes -- -> [2,3,5,7,11,13,17,19,23,29]
    --print $ take 10 primes'
 
    --print $ take 10 facts -- -> [1,1,2,6,24,120,720,5040,40320,362880]
    --print $ take 10 facts'
    --print $ take 10 facts''
 
    --print $ take 10 fibs  -- -> [1,1,2,3,5,8,13,21,34,55]
    --print $ take 10 fibs'
 
    --print $ take 3 pythagTriples' -- -> [(3,4,5),(6,8,10),(5,12,13)]
 
    --print $ member 3 [1,3..]    -- -> True
    --print $ member 2 [1,3..]    -- бездънна рекурсия!!! - трябва ръчно да се спре изпълнението на кода
    --print $ memberOrd 2 [1,3..] -- -> False