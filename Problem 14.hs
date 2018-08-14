import Control.Arrow ((&&&))
import Data.Function.Memoize (memoFix)
import Data.List (maximumBy)
import Data.Ord (comparing)

collatzM :: Int -> Int
collatzM = memoFix collatz

collatz _ 1 = 1
collatz f n = (f $ if even n then n `div` 2 else 3*n+1)+1

main = print $ maximumBy (comparing collatzM) [1..10^6-1]