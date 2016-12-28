import Data.Array
import MathFunctions (isqrt)

--let n=25 in foldl (\arr x -> accum (flip (:)) arr $ zip [x^2+x, x^2+2*x..n] $ repeat x) (listArray (1, n) $ repeat []) [1..isqrt n]

upperBound = 10^6 `div` 4

main = print $ foldl (\ans x -> length [x^2+x, x^2+2*x..upperBound] + ans) 0 [1..isqrt upperBound]