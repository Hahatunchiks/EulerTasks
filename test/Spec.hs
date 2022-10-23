module Main (main) where
import System.Exit

import FirstSolution
import SecondSolution
import ThirdSolution
import FourthSolution
import FifthSolution


run4TaskTests1 :: Bool
run4TaskTests1 =  solution4TailRec == 906609 && solution4ModuleImpl == 906609 && solution4Map == 906609 && solution4Rec == 906609 && solution4InfList == 906609

run27TaskTest1 :: Bool
run27TaskTest1 = solution27TailRec == (-35) && solution27Rec == (-35) && solution27ModuleImpl == (-35) && solution27Map == (-35) && solution27InfList == (-35)

main :: IO()
main =  if run4TaskTests1 && run27TaskTest1 then
        print "Tests passed"
    else
        die "didn't passed"
