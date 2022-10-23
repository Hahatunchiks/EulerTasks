module Main (main) where
import FirstSolution


firstTest::Int
firstTest = if solution4TailRec == 906609 then 0 else 1

main :: IO()
main = print firstTest
