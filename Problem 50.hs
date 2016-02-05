import Data.List (maximumBy, tails)
import Data.Ord (comparing)
import MathFunctions (primes, isPrime)

main = print $ snd $ maximumBy (comparing fst) $ concatMap (take 1 . filter (isPrime . snd) . reverse . zip [1..] . takeWhile (<10^6) . scanl1 (+)) $ take 10 $ tails primes