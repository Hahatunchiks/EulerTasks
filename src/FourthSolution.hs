{-# LANGUAGE MultiWayIf #-}

module FourthSolution  where

-- 4 problem 

isPalindrome :: Int -> Bool
isPalindrome n = let s = show n in s == reverse s


largestPalindrome :: Int -> Int -> Int
largestPalindrome x y  =
    let curr = if ( x <= 999 && y <= 999 && isPalindrome (x*y) ) then x*y else 0
    in
        if  | x > 999 -> curr
            | y > 999 -> max curr $ largestPalindrome (x+1) 1
            | otherwise -> max curr $ largestPalindrome x (y+1)

solution4 :: Int
solution4 = largestPalindrome 1 1

--27 problem

factors :: Int -> [Bool]
factors x = 
    if  | x <= 0 -> [True] 
        | otherwise -> map (\y -> (x `mod` y) == 0) [2..x-1]


isPrime :: Int -> Bool  
isPrime x = not (foldl (||) False (factors x))


calcFormula :: Int -> Int -> Int -> Int
calcFormula a b x = x*x + a*x + b


calcPrimes :: Int -> Int -> Int -> Int
calcPrimes a b x  = 
    let n = calcFormula a b x
    in
        if  | isPrime n -> calcPrimes a b (x+1)
            | otherwise ->  x


quadraticPrimes :: Int -> Int -> Int -> Int -> Int
quadraticPrimes a b result product = 
    let amount = calcPrimes a b 0
        newResult = if a < 1000 && b <= 1000 && amount > result then amount else result
        newProduct = if a < 1000 && b <= 1000 && amount > result then a*b else product
    in
    if  | a >= 1000 -> newProduct
        | b > 1000 -> quadraticPrimes (a + 1) (-1000) newResult newProduct
        | otherwise -> quadraticPrimes a (b+1) newResult newProduct




solution27 :: Int
solution27 = quadraticPrimes (-999) (-1000) 0 0
