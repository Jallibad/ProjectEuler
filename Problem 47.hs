import Data.List
import MathFunctions

distinctPrimeFactorsGenerator :: Int -> Int
distinctPrimeFactorsGenerator d = fst $ until (all (==d) . take d . snd) (\(x, (_:ps)) -> (x+1, ps)) (1, map (length . distinctPrimeFactors) [1..])

main = print $ distinctPrimeFactorsGenerator 4