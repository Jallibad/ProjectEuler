import Data.List (maximumBy, tails)
import Data.Ord (comparing)
import MathFunctions (primes, isPrime)

upperBound = 10^6

main = print $ snd $ maximumBy (comparing fst) $ concatMap (take 1 . filter (isPrime . snd) . reverse . zip [1..] . takeWhile (<upperBound) . scanl1 (+)) $ take 10 $ tails primes