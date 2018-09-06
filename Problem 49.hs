import ListFunctions (allEqual, differences, pick)
import PrimeFunctions (isPrime)
import Data.List

possibilities :: [[Int]]
possibilities = [nub $ sort $ filter isPrime $ map read $ permutations [a,b,c,d] | d <- ['4'..'9'], c <- ['3'..d], b <- ['2'..c], a <- ['1'..b]]

main = print $ delete "148748178147" [concatMap show x | x <- concatMap (pick 3) possibilities, allEqual $ differences x]