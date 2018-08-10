import Data.Array.IO (getAssocs, IOUArray)
import Data.Function (on)
import Data.List (maximumBy)
import Data.Ord (comparing)
import Sieve (totientSieve)

main = (totientSieve $ 10^6 :: IO (IOUArray Int Int)) >>= getAssocs >>=
	print . fst . maximumBy cmp . tail
	where cmp = comparing $ uncurry $ on (/) fromIntegral