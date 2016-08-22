import MathFunctions (isPrime)
import Data.List (permutations)
import Data.Char (intToDigit)

intListToInt :: [Int] -> Int
intListToInt = read . map intToDigit

--primeSet f [] = [f]
--primeSet f xs = concat [primeSet (a:f) b | x <- [1..length xs], let (a',b) = splitAt x xs, let a = intListToInt a', ((null f) || a > (head f)) && isPrime a]

primeSet _ [] = 1
primeSet f xs = sum [primeSet a b | x <- [1..length xs], let (a',b) = splitAt x xs, let a = intListToInt a', a > f && isPrime a]

main = print $ sum $ map (primeSet 0) $ permutations [1..9]