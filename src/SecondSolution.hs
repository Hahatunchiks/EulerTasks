{-# LANGUAGE MultiWayIf #-}

module SecondSolution  (solution4ModuleImpl, solution27ModuleImpl )where
import Utils
{-
   Solutions with separated modules: sequence generation, sequence filtering, sequence folding
-}

-- 4 problem: Largest palindrome product

-- generate a sequence of products [100..999] X [ 100..999]
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



-- 27 problem: Quadratic primes

generateSeq :: [(Int, Int)] -> Int -> Int -> [(Int, Int)]
generateSeq list a b = 
    if  | a < (-9) -> list
        | b < (-10) -> generateSeq  ((a*b, calcPrimes a b 0) : list) (a-1) 10
        | otherwise -> generateSeq   ((a*b, calcPrimes a b 0) : list) a (b-1)

max' :: (Int, Int) -> (Int, Int) -> (Int, Int)
max' x y = 
    if  snd x > snd y then x else y

solution27ModuleImpl :: Int
solution27ModuleImpl = 
    let 
        generated = generateSeq [] 9 10
    in
        fst $ foldl1 max' generated
