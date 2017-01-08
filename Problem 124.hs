import MathFunctions (distinctPrimeFactors)
import Data.List (sortOn)
import Data.Ord (comparing)

rad = product . distinctPrimeFactors

main = print $ (sortOn rad [1..10^5]) !! (10^4-1)