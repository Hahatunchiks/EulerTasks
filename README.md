Министерство науки и высшего образования Российской Федерации федеральное государственное автономное образовательное учреждение высшего образования

«Национальный исследовательский университет ИТМО»

---
__ФПИиКТ, Системное и Прикладное Программное Обеспечение__

__Лабораторная работа №1__

по Функциональному программированию

Выполнил: Лавлинский М. С.

Группа: P34112

Преподаватель: Пенской Александр Владимирович

###### Санкт-Петербург
###### 2022 г.

---

## Описание проблем

### Problem 4

#### [Largest palindrome product](https://projecteuler.net/problem=4)

### Problem 27

#### [Quadratic primes](https://projecteuler.net/problem=27)




## Ключевые элементы реализации с минимальными комментариями

### Problem 4

### Вспомогательные функции.
    ```
    -- check that given number is palindrome
    isPalindrome :: Int -> Bool
    isPalindrome n = let s = show n in s == reverse s

    -- return largest palindrome product for x * [999.100]
    largestPalindrome :: Int -> Int -> Int
    largestPalindrome x y  =

        if  | x > 999  -> (-1)
            | y <= 99 -> 0
            | isPalindrome (x*y) -> x * y
            | otherwise -> largestPalindrome x (y-1)
    ```


1. __монолитная реализация__
    + __хвостовая рекурсия__
    
    ```
    -- tail recursive solution

    -- return largest palindrome product for [x..999] * [999.100]
    largestPalindromeProduct :: Int -> Int-> Int
    largestPalindromeProduct x acc = 
        if  x == 1000 then acc else largestPalindromeProduct (x+1) (max (largestPalindrome x 999) acc)

    -- return largest palindrome product for [100..999] * [999.100]
    solution4TailRec :: Int
    solution4TailRec = largestPalindromeProduct 100 0
    ```
    + __рекурсия__

    ```
    -- return largest palindrome product for [x..999] * [y..999]
    largestPalindrome' :: Int -> Int -> Int
    largestPalindrome' x y  =
        let curr = if x <= 999 && y <= 999 && isPalindrome (x*y)  then x*y else 0
        in
            if  | x > 999 -> curr
                | y > 999 -> max curr $ largestPalindrome' (x+1) 100
                | otherwise -> max curr $ largestPalindrome' x (y+1)

    -- return largest palindrome product for [100..999] * [999.100]
    solution4Rec :: Int
    solution4Rec = largestPalindrome' 100 100

    ```

2. __модульная реализация__

    ```
    generateProducts :: [Int] -> Int -> Int -> [Int]
    generateProducts  list x y =
        if  | y > 999 -> generateProducts list (x+1) 100
            | x > 999 -> list
            | otherwise -> generateProducts ((x*y) : list) x (y+1)

    -- filter our products, return palindromes
    filterProducts :: [Int] -> [Int]
    filterProducts = filter isPalindrome 

    -- get maximal palindrome 
    solution4ModuleImpl :: Int
    solution4ModuleImpl = foldl max 0 $ filterProducts $ generateProducts [] 100 100
    ```

3. __генерация последовательности при помощи отображения (map)__

    ```
    solution4Map :: Int
    solution4Map  = maximum $ map (`largestPalindrome` 999) [100..999]
    ```

5. __работа с бесконечными списками для языков поддерживающих ленивые коллекции или итераторы как часть языка__

    ```
    -- handle one by one element from infinite list, while  number less than 999
    largestPalindromeProduct :: [Int] -> [Int] -> [Int] 
    largestPalindromeProduct input palindromes = 
        let
            x = largestPalindrome (head input) 999
        in
            if   x == (-1) then palindromes else largestPalindromeProduct (tail input) (x : palindromes)

    solution4InfList :: Int
    solution4InfList  = maximum $ largestPalindromeProduct [1..] []
    ```

6. __реализация на любом удобном языке программировании__

    ``` 
        int LargestPalindromeProduct() {
        int largest = 0, num, mod, numCopy, reversed;

        for(int i = 1; i <= 999; i++) {
            for(int j = 1; j <= i; j++) {
                reversed = 0;
                num = i*j;
                numCopy = num;

                do {
                    mod = num % 10;
                    reversed = (reversed * 10) + mod;
                    num = num / 10;
                } while (num > 0);

                if(reversed == numCopy && reversed > largest) {
                    largest = reversed;
                }
            }
        }

        return largest;
    }

    ```
Функция перебирает все пары чисел в диапазоне [1..999] и возвращает наибольшее произведение, являющееся палиндромом

### Problem 29

1. __монолитная реализация__
    + __хвостовая рекурсия__
    ```
    
    quadraticPrimes :: Int -> Int -> Int -> Int -> Int -> Int -> Int
    quadraticPrimes a b x  result product maxR = 
        let 
            maxRes = if result > maxR then result else maxR
            newProduct = if result > maxR then a*b else product
            n = calcFormula a b x
        in
            if  | isPrime n -> quadraticPrimes a b (x+1) (result+1) newProduct maxRes
                | a < (-10) -> newProduct
                | b <= (-10)  -> quadraticPrimes (a-1) 10 0 0 newProduct maxRes
                | otherwise -> quadraticPrimes a (b-1) 0 0 newProduct maxRes

    solution27TailRec :: Int
    solution27TailRec = quadraticPrimes 9 10 0 0 0 0
    ```

    + __рекурсия__
    ```
    --
    quadraticPrimes' :: Int -> Int -> Int -> Int -> Int
    quadraticPrimes' a b result productMy = 
        let amount = calcPrimes a b 0
            newResult = if a < 10 && b <= 10 && amount > result then amount else result
            newProduct = if a < 10 && b <= 10 && amount > result then a*b else productMy
        in
        if  | a >= 10 -> newProduct
            | b > 10 -> quadraticPrimes' (a + 1) (-10) newResult newProduct
            | otherwise -> quadraticPrimes' a (b+1) newResult newProduct

    solution27Rec :: Int
    solution27Rec = quadraticPrimes' (-9) (-10) 0 0
    ```

2. __модульная реализация__
    ```

    -- generating a sequence of the number of primes for all a and b
    generateSeq :: [(Int, Int)] -> Int -> Int -> [(Int, Int)]
    generateSeq list a b = 
        if  | a < (-9) -> list
            | b < (-10) -> generateSeq  ((a*b, calcPrimes a b 0) : list) (a-1) 10
            | otherwise -> generateSeq   ((a*b, calcPrimes a b 0) : list) a (b-1)

    max' :: (Int, Int) -> (Int, Int) -> (Int, Int)
    max' x y = 
        if  snd x > snd y then x else y

    solution27ModuleImpl :: Int
    solution27ModuleImpl = 
        let 
            generated = generateSeq [] 9 10
        in
            fst $ foldl1 max' generated
    ```

3. __генерация последовательности при помощи отображения (map)__
    ```
    -- generating a sequence of the number of primes for all a and b
    generateSeq ::  [[(Int, Int)]]
    generateSeq  = map (\a -> map (\b -> (a*b,calcPrimes a b 0)) [(-10)..10] ) [(-9)..9] 

    -- compare two pairs by the number of primes
    max' :: (Int, Int) -> (Int, Int) -> (Int, Int)
    max' x y = 
        if  snd x > snd y then x else y

    -- get maximal element from list, using max' for compare
    maxInList :: [(Int, Int)] -> (Int, Int)
    maxInList  =  foldl1 max'

    solution27Map :: Int
    solution27Map = fst $ foldl max' (0,0) $ map maxInList generateSeq
    ```

5. __работа с бесконечными списками для языков поддерживающих ленивые коллекции или итераторы как часть языка__
    ```
    -- returns the number of first sequental primes
    calcPrimes' :: [Int] -> Int -> Int -> Int
    calcPrimes' input a b  = 
        let 
            x = head input
            n = if x <= 1000 then calcFormula a b x else (-1)
        in
            if  isPrime n then calcPrimes' (tail input) a b else  x
    
    -- recursive change a and b and return a*b for maximal calcPrimes'
    quadraticPrimes' :: Int -> Int -> Int -> Int -> Int
    quadraticPrimes' a b result productMy = 
        let amount = calcPrimes' [0..] a b 
            newResult = if a < 1000 && b <= 1000 && amount > result then amount else result
            newProduct = if a < 1000 && b <= 1000 && amount > result then a*b else productMy
        in
        if  | a >= 1000 -> newProduct
            | b > 1000 -> quadraticPrimes' (a+1) (-1000) newResult newProduct
            | otherwise -> quadraticPrimes' a (b+1) newResult newProduct


    solution27InfList :: Int
    solution27InfList = quadraticPrimes' (-999) (-1000) 0 0
    ```

6. __реализация на любом удобном языке программировании__
    ```
    
    ```

## Выводы
