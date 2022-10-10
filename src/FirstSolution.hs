{-# LANGUAGE MultiWayIf #-}

module FirstSolution  where

--4 problem

isPalindrome :: Int -> Bool
isPalindrome n = let s = show n in s == reverse s


largestPalindrome :: Int -> Int -> Int
largestPalindrome x y  =
    if  | y <= 0 -> 0
        | isPalindrome (x*y) -> x * y
        | otherwise -> largestPalindrome x (y-1)


largestPalindromeProduct :: Int -> Int-> Int
largestPalindromeProduct x acc = 
    if  | x == 1000 -> acc
        | otherwise -> largestPalindromeProduct (x+1) (max (largestPalindrome x 999) acc)


solution4 :: Int
solution4 = largestPalindromeProduct 1 0


--27 problem

factors :: Int -> [Bool]
factors x = 
    if  | x <= 0 -> [True] 
        | otherwise -> map (\y -> (x `mod` y) == 0) [2..x-1]


isPrime :: Int -> Bool  
isPrime x = not (foldl (||) False (factors x))


calcFormula :: Int -> Int -> Int -> Int
calcFormula a b x = x*x + a*x + b


calcPrimes :: Int -> Int -> Int -> Int -> Int -> Int -> Int
calcPrimes a b x  result product maxR = 
    let 
        maxRes = if result > maxR then result else maxR
        newProduct = if result > maxR then a*b else product
        n = calcFormula a b x
    in
        if  | isPrime n -> calcPrimes a b (x+1) (result+1) newProduct maxRes
            | a >= 1000 -> newProduct
            | b > 1000  -> calcPrimes (a+1) (-1000) 0 0 newProduct maxRes
            | otherwise -> calcPrimes (a) (b+1) 0 0 newProduct maxRes


solution27 :: Int
solution27 = calcPrimes (-999) (-1000) 0 0 0 0