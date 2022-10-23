{-# LANGUAGE MultiWayIf #-}

module FourthSolution (solution4Rec, solution27Rec)  where
import Utils
-- 4 problem 
-- recursive solution

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

--27 problem
-- recursive solution

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
