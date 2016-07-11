import Data.List
import ListFunctions
import MathFunctions

upperBound = 10^8

squares = map (^2) [1..isqrt (upperBound `div` 2)]

main = print $ sum $ nub $ filter (isPalindrome . show) $ concatMap (takeWhile (<upperBound) . map sum . drop 2 . inits) $ tails squares
		