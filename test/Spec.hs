module Main (main) where
import System.Exit

import FirstSolution
import SecondSolution
import ThirdSolution
import FourthSolution
import FifthSolution


run4TaskTests1 :: Bool
run4TaskTests1 =  solution4TailRec == 906609 && solution4ModuleImpl == 906609 && solution4Map == 906609 && solution4Rec == 906609 && solution4InfList == 906609

main :: IO()
main =  if run4TaskTests1 then
        print "Tests passed"
    else
        die "didn't passed"
