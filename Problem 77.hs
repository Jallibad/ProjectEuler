import Control.Arrow ((&&&))
import Data.Function.Memoize (memoFix)
import MathFunctions (primes)

primeSums :: Integer -> Integer
primeSums = (memoFix primeSums') . (id &&& pred)

primeSums' :: Integral a => ((a, a) -> a) -> (a, a) -> a
primeSums' _ (0, _) = 1
primeSums' f (n, max) = sum $ map addPrime $ takeWhile (<=(min n max)) primes
	where
		addPrime x = f (n-x, min x $ n-x)

main = print $ until ((>5000) . primeSums) (+1) 1