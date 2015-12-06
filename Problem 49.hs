import Data.List
import MathFunctions
import ListFunctions

fourDigitPrimes = filter isPrime [10^3..10^4-1]

possibilities :: [[Int]]
possibilities = map sort $ filter ((>=3) . length) [nub $ filter isPrime $ map read $ permutations [a,b,c,d] | d <- ['4'..'9'], c <- ['3'..d], b <- ['2'..c], a <- ['1'..b]]

main = print $ (map (concatMap show) $ filter (allEqual . differences) $ concatMap (pick 3) possibilities) \\ ["148748178147"]