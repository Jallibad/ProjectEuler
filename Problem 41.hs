import Data.List
import MathFunctions

main = print $ maximum $ filter isPrime $ map (\x -> read x :: Int) $ permutations ['7','6'..'1']