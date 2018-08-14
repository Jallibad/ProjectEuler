import qualified Data.Set as Set
import MathFunctions (factors)
import Sieve
import Data.Array.Unboxed

abundants :: Set.Set Int
abundants = Set.fromDistinctAscList $ map fst $ filter (uncurry (<)) $ assocs (divisorSieve (+) 0 28123 :: UArray Int Int)

isAbundantSum x = any (\i -> Set.member (x-i) abundants) $ fst $ Set.split x abundants

main = print $ sum $ filter (not . isAbundantSum) [1..28123]