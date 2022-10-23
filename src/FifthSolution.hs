{-# LANGUAGE MultiWayIf #-}

module FifthSolution (solution4InfList, solution27InfList) where
import Utils

largestPalindromeProduct :: [Int] -> [Int] -> [Int] 
largestPalindromeProduct input palindromes = 
    let
        x = largestPalindrome (head input) 999
    in
        if   x == (-1) then palindromes else largestPalindromeProduct (tail input) (x : palindromes)

solution4InfList :: Int
solution4InfList  = maximum $ largestPalindromeProduct [1..] []


-- 27 problem

calcPrimes' :: [Int] -> Int -> Int -> Int
calcPrimes' input a b  = 
    let 
        x = head input
        n = if x <= 1000 then calcFormula a b x else (-1)
    in
        if  isPrime n then calcPrimes' (tail input) a b else  x
    

quadraticPrimes' :: Int -> Int -> Int -> Int -> Int
quadraticPrimes' a b result productMy = 
    let amount = calcPrimes' [0..] a b 
        newResult = if a < 10 && b <= 10 && amount > result then amount else result
        newProduct = if a < 10 && b <= 10 && amount > result then a*b else productMy
    in
    if  | a >= 10 -> newProduct
        | b > 10 -> quadraticPrimes' (a+1) (-10) newResult newProduct
        | otherwise -> quadraticPrimes' a (b+1) newResult newProduct


solution27InfList :: Int
solution27InfList = quadraticPrimes' (-9) (-10) 0 0
