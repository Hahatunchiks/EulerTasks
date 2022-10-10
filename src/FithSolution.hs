{-# LANGUAGE MultiWayIf #-}

module ThirdSolution where

isPalindrome :: Int -> Bool
isPalindrome n = let s = show n in s == reverse s


largestPalindrome :: Int -> Int -> Int
largestPalindrome x y  =
    if  | y <= 0 -> 0
        | isPalindrome (x*y) -> x * y
        | otherwise -> largestPalindrome x (y-1)


solution :: Int
solution  = maximum $ map (\x -> largestPalindrome x 999) [1..999]
