{-# LANGUAGE MultiWayIf #-}

module ThirdSolution where

-- 4 problem 
isPalindrome :: Int -> Bool
isPalindrome n = let s = show n in s == reverse s


largestPalindrome :: Int -> Int -> Int
largestPalindrome x y  =
    if  | y <= 0 -> 0
        | isPalindrome (x*y) -> x * y
        | otherwise -> largestPalindrome x (y-1)


solution :: Int
solution  = maximum $ map (\x -> largestPalindrome x 999) [1..999]


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


genRow :: Int -> [Int]
genRow a = map (\b -> calcPrimes a b 0) [-10..10]

genTable :: Int -> [Int] -> [Int]
genTable a table = 
    if  | a < 999 -> genTable (a+1) ((genRow a) ++ table) 
        | otherwise -> table

solution27 :: IO()
solution27 = print (genTable 0 [])