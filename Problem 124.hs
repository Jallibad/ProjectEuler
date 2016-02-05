import MathFunctions
import Data.List
import Data.Ord

rad = product . distinctPrimeFactors

main = print $ (sortBy (comparing rad) [1..10^5]) !! (10^4-1)