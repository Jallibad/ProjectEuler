import MathFunctions (distinctPrimeFactors)
import Data.List (sortBy)
import Data.Ord (comparing)

rad = product . distinctPrimeFactors

main = print $ (sortBy (comparing rad) [1..10^5]) !! (10^4-1)