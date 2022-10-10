{-# LANGUAGE MultiWayIf #-}

module SecondSolution (solution) where

isPalindrome :: Int -> Bool
isPalindrome n = let s = show n in s == reverse s

largestPalindrome :: Int -> Int -> Int
largestPalindrome x y  =
    if  | y <= 0 -> 0
        | isPalindrome (x*y) -> x * y
        | otherwise -> largestPalindrome x (y-1)


generatePalindromes :: [Int]
generatePalindromes  = map (\x -> largestPalindrome x 999) [1..999]


max' :: Int -> Int -> Int
max' x y =
    if  |  x >= y -> x
        | otherwise -> y


solution :: Int
solution = foldl max' 0 generatePalindromes
