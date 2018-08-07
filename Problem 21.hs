import Data.Array.Unboxed (UArray)
import qualified MathFunctions (amicable)
import Sieve (divisorSieve)
import Memoize (lookupFrom)

amicable = MathFunctions.amicable `lookupFrom` (divisorSieve (+) 0 $ 10^4 :: UArray Int Int)

isAmicable :: Int -> Bool
isAmicable a = a == amicable b && a /= b
	where b = amicable a

main = print $ sum $ filter isAmicable [1..10^4-1]