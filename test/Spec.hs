module Main (main)   where

import FirstSolution
import SecondSolution
import ThirdSolution
import FourthSolution
import FifthSolution

import Test.Tasty
import Test.Tasty.HUnit 

test1 :: TestTree
test1 = testCase "test tail rec solution, 4 problem" $ assertEqual [] solution4TailRec 906609

test2 :: TestTree
test2 = testCase "test module impl solution, 4 problem" $ assertEqual [] solution4ModuleImpl 906609

test3 :: TestTree
test3 = testCase "test solution uses map, 4 problem" $ assertEqual [] solution4Map 906609

test4 :: TestTree
test4 = testCase "test recursive solution, 4 problem" $ assertEqual [] solution4Rec 906609

test5 :: TestTree
test5 = testCase "test solution uses infinite list, 4 problem " $ assertEqual [] solution4InfList 906609

test6 :: TestTree
test6 = testCase "test tail rec solution, 27 task" $ assertEqual [] solution27TailRec (-35)

test7 :: TestTree
test7 = testCase "test module impl solution, 27 problem" $ assertEqual [] solution27ModuleImpl (-35)

test8 :: TestTree
test8 = testCase "test solution uses map, 27 problem" $ assertEqual [] solution27Map (-35)

test9 :: TestTree
test9 = testCase "test recursive solution, 27 problem" $ assertEqual [] solution27Rec (-35)

test10 :: TestTree
test10 = testCase "test solution uses infinite list, 27 problem" $ assertEqual [] solution27InfList (-35)

tests :: TestTree
tests = testGroup "Unit tests, Largest Palindrome Product" [test1, test2, test3, test4, test5, test6, test7, test8, test9, test10]

main :: IO()
main = defaultMain tests