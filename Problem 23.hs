import qualified Data.Set as Set
import MathFunctions (factors)
import Sieve
import Data.Array.Unboxed

abundants' = filter (\x -> (sum $ init $ factors x) > x) [1..28123-12]
abundants = map fst $ filter (uncurry (<)) $ assocs (divisorSieve (+) 0 28123 :: UArray Int Int)

abundantSet = Set.fromDistinctAscList abundants

isAbundantSum x = any (\i -> Set.member (x-i) abundantSet) $ takeWhile (<x) abundants

main = print $ sum $ filter (not . isAbundantSum) [1..28123]