import Control.Applicative
import PrimeFunctions (isCircularPrime)

main = print $ length $ filter isCircularPrime [1..10^6]