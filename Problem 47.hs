import Control.Arrow
import Data.List (genericLength)
import MathFunctions (distinctPrimeFactors)

distinctPrimeFactorsGenerator :: Int -> Int
distinctPrimeFactorsGenerator d = firstPart $ all (==d) . take d

thing :: (Enum a, Num a) => [(a, [a])]
thing = zip [1..] $ iterate tail $ map (genericLength . distinctPrimeFactors) [1..]

firstPart :: (Enum a, Num a) => ([a] -> Bool) -> a
firstPart = fst . head . flip filter thing . (snd >>>)

main = print $ distinctPrimeFactorsGenerator 3