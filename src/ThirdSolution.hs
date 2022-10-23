module ThirdSolution (solution27Map, solution4Map) where
import Utils

{-
    Solution use map to generate sequence
-}

-- 4 problem 


solution4Map :: Int
solution4Map  = maximum $ map (`largestPalindrome` 999) [100..999]


--27 problem

-- generating a sequence of the number of primes for all a and b
generateSeq ::  [[(Int, Int)]]
generateSeq  = map (\a -> map (\b -> (a*b,calcPrimes a b 0)) [(-10)..10] ) [(-9)..9] 

-- compare two pairs by the number of primes
max' :: (Int, Int) -> (Int, Int) -> (Int, Int)
max' x y = 
    if  snd x > snd y then x else y

-- get maximal element from list, using max' for compare
maxInList :: [(Int, Int)] -> (Int, Int)
maxInList  =  foldl1 max'

solution27Map :: Int
solution27Map = fst $ foldl max' (0,0) $ map maxInList generateSeq
