{-# LANGUAGE MultiWayIf #-}

module FirstSolution  where
import Utils

--4 problem
-- tail recursive solution

-- return largest palindrome product for [x..999] * [999.100]
largestPalindromeProduct :: Int -> Int-> Int
largestPalindromeProduct x acc = 
    if  x == 1000 then acc else largestPalindromeProduct (x+1) (max (largestPalindrome x 999) acc)

-- return largest palindrome product for [100..999] * [999.100]
solution4 :: Int
solution4 = largestPalindromeProduct 100 0


--27 problem
-- tail recursive solution

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


solution27 :: Int
solution27 = quadraticPrimes 9 10 0 0 0 0