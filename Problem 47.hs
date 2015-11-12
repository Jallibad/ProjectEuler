import Data.List
import MathFunctions

primeFactorLengths = map (length . distinctPrimeFactors) [1..]

distinctPrimeFactorsGenerator :: Int -> Int
distinctPrimeFactorsGenerator d = until (\x -> all (==d) $ take d $ drop (x-1) primeFactorLengths) (+1) 1

main = print $ distinctPrimeFactorsGenerator 4