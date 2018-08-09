import MathFunctions (isPrime)
import Data.List (permutations)

main = print $ maximum $ filter isPrime $ map read $ permutations ['1'..'7']