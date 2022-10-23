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
Функция вернет генератор, в котором все числа фибоначчи до заданного лимита. А функции `filter` и `sum` соответственно отсеют нечетные и найдут сумму оставшихся чисел.

### Problem 29

1. __монолитная реализация__
    + __хвостовая рекурсия__
    ```
    (define (loop-a limit a b unique)
      (if (> a limit) unique (loop-a limit (add1 a) b (set-add unique (expt a b)))))

    (define (loop-b high-limit lower-limit b unique)
      (if (> b high-limit)
          unique
          (loop-b
            high-limit
            lower-limit
            (add1 b)
            (loop-a high-limit lower-limit b unique))))

    (define (distinct-powers left right)
      (if (< right left)
          (raise (make-exn:fail:contract "Invalid parameters passed"))
          (set-count (loop-b right left left (set)))))

    (distinct-powers 2 100)
    ```
    В параметрах `a` и `b` `loop-a` и `loop-b` указываются счетчики циклов. А в параметре `unique` собирается результирующее множество чисел $a^b$ путем объединения с уже полученными множествами на предыдущих итерациях. Затем находим мощность множества с помощью `set-count`.

    + __рекурсия__
    ```
    (define (inner-loop init limit b)
      (define a init)
      (if (= a limit)
        (set (expt a b))
        (set-add (inner-loop (add1 a) limit b) (expt a b))))

    (define (external-loop init-b init-a limit-b limit-a)
      (define b init-b)
      (if (= b limit-b)
          (inner-loop init-a limit-a b)
          (set-union
            (external-loop (add1 b) init-a limit-b limit-a)
            (inner-loop init-a limit-a b))))

    (define (distinct-powers left right)
      (if (negative? right)
          (raise (make-exn:fail:contract "Invalid right value passed"))
          (set-count (external-loop left left right right))))

    (distinct-powers 2 100)
    ```
    Рекурсивная реализация строит множество с помощью двух функций, одна для внешнего цикла – степени числа, другая для вложенного цикла – основания. У результирующего множества находим затем его мощность.

2. __модульная реализация__
```
(define (distinct-powers left right)
  (foldl +
         0
         (map (lambda (n) (quotient n n))
              (remove-duplicates
                (let ([powers (build-list (add1 (- right left))
                                                           (lambda (n) (+ n left)))])
                                   (for*/list ([a powers] [b powers])
                                     (expt a b)))))))

(distinct-powers 2 100)
```
Во вложенном цикле собираем список из всевозможных комбинаций `a` и `b`. Убираем повторения и с помощью `(quotient n n)` заменяем все числа на `1`, сумма этих единиц и даст длину списка, что является ответом на задачу.

3. __генерация последовательности при помощи отображения (map)__
```
(define (distinct-powers left right)
  (foldl +
         0
         (map (lambda (n) (quotient n n))
              (remove-duplicates
                (append* (let ([powers (inclusive-range left right)])
                                            (for/list ([a powers])
                                              (map (lambda (n) (expt n a)) powers))))))))

(distinct-powers 2 100)
```
С помощью самого внутреннего вызова map мы формируем получаем список чисел вида $a^n$, где `n` меняется от `2` до `100` в цикле `for/list`, и все полученные списки организуются в один список, получаются списки в списке, который приводим к одномерному списку с помощью формы `append*`. Следующий `map` нужен, чтобы заменить все уникальные числа вида $a^b$ на `1`, хитрый вызов `(quotient n n)` нужен только, чтобы линтер не ругался, что в лямбде не используется параметр `n`. И полученный список из `1`, сворачиваем в сумму, что и будет длиной этого списка и ответом на задачу.

4. __работа со спец. синтаксисом для циклов__
```
(define (distinct-powers left right)
  (for/fold ([length 0])
            ([distinct-power
              (for/fold ([acc '()])
                        ([a-b (let ([powers (build-list (add1 (- right left))
                                                        (lambda (n) (+ n left)))])
                                (for*/list ([a powers] [b powers])
                                  (expt a b)))])
                (if (= (length (filter (lambda (x) (equal? x a-b)) acc)) 0)
                  (list* a-b acc)
                  acc))])
    (if (number? distinct-power)
        (add1 length)
        (raise-argument-error 'incorrect-type "number?" distinct-power))))

(distinct-powers 2 100)
```
Специальный цикл `for*/list` позволяет пройтись по всем комбинациям поданных итерируемых коллекциях и собрать все это в единый список. Затем с помощью цикла `for/fold` мы собираем это в еще один список, убрав все повторения путем того, что мы в нашем собираемом списке проверяем, есть ли уже данный элемент. И только затем, с помощью еще одного цикла `for/fold`, мы получаем количество чисел в полученном множестве.

5. __работа с бесконечными списками для языков поддерживающих ленивые коллекции или итераторы как часть языка__
```
(define (distinct-powers left right)
  (stream-length (for/stream ([i
                               (remove-duplicates
                                (let ([powers (in-inclusive-range left right)])
                                  (stream->list (for*/stream ([a powers] [b powers]) (expt a b)))))])
                             i)))

(distinct-powers 2 100)
```
С помощью `for*/stream` организуется вложенный цикл по всем комбинациям `a` и `b`, которые затем собираются в стрим, который преобразуется в список, из которого мы убираем повторения, затем снова собираем список в стрим и находим его длину. Моя попытка решения с использованием только стримов и собственной функции удаления повторений приводила к слишком долгим вычислениям в частности из-за ленивости стримов, который вычислялся только в конце, когда вызывалась функция `stream-length`.

6. __реализация на любом удобном языке программировании__
```
def distinct_powers(a_bounds: Tuple[int, int] = (0, 0),
        b_bounds: Tuple[int, int] = (0, 0)) -> Set[int]:
    a_left, a_right = a_bounds
    b_left, b_right = b_bounds
    result = set()
    for base_a in range(a_left, a_right + 1):
        for exponent_b in range(b_left, b_right + 1):
            result.add(base_a**exponent_b)
    return result


def main():
    bounds = (2, 100)
    print(len(distinct_powers(bounds, bounds)))
```
Решение в лоб. Во вложенном цикле добавляем $a^b$ в множество, тем самым получаем множество чисел без повторений и просто берем его мощность.

## Выводы

В ходе выполнения данной лабораторной работы я освоил базовые приемы и абстракции языка __Racket__. Рекурсия, функции свертки, отображения и другие потоковые функции, списки, последовательности и стримы. Формы передачи потока управления в виде различных циклов, условных форм `if` и `cond`. Формы для организации локальных привязок с помощью `let`.