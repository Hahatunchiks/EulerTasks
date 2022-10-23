{-# LANGUAGE MultiWayIf #-}
module Utils  
(
    isPalindrome,
    largestPalindrome,
    factors,
    isPrime,
    calcFormula,
    calcPrimes
)where

-- check that given number is palindrome
isPalindrome :: Int -> Bool
isPalindrome n = let s = show n in s == reverse s

-- return largest palindrome product for x * [999.100]
largestPalindrome :: Int -> Int -> Int
largestPalindrome x y  =

    if  | x > 999 -> (-1)
        | y <= 99 -> 0
        | isPalindrome (x*y) -> x * y
        | otherwise -> largestPalindrome x (y-1)


-- return list of factors
factors :: Int -> [Bool]
factors x = 
    if  x <= 0 then [True] else map (\y -> (x `mod` y) == 0) [2..x-1]


or' :: Bool -> Bool -> Bool
or' x y = x || y

-- check that given number is prime
isPrime :: Int -> Bool  
isPrime x = not (foldl or' False (factors x))

-- calculate formula - x^2 + a*x + b
calcFormula :: Int -> Int -> Int -> Int
calcFormula a b x = x*x + a*x + b

-- get the size of the sequence
calcPrimes :: Int -> Int -> Int -> Int
calcPrimes a b x  = 
    let n = calcFormula a b x
    in
        if  isPrime n then calcPrimes a b (x+1) else x
