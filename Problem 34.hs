import Data.Char

maxNumber :: Int
maxNumber = 10^(until (\x -> (362880*x) < (10^x-1)) (+1) 1)-1

factorials = 1 : 1 : scanl1 (*) [2..]

digitFactorial :: Integral (a) => Show (a) => a -> Int
digitFactorial number = sum $ map (\x -> factorials !! digitToInt x) $ show number

main = print $ sum $ filter (\x -> x == digitFactorial x) [3..maxNumber]