import Data.List
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import MathFunctions (isPrime, primes)

sumConsecPrimes n = find (isPrime . snd) . reverse . zip [1..] . takeWhile (<n) . scanl1 (+)

main = print $ snd $ maximumBy (comparing fst) $ mapMaybe (sumConsecPrimes $ 10^6) $ take 10 $ tails primes