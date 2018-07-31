import Data.Function
import Data.List
import Data.Ratio
import qualified Data.Map as M
import MathFunctions
import Data.Ord (comparing)
import Data.Array.ST
import Control.Monad.ST
import Control.Monad
import Control.Arrow

type TotientSieve s = STUArray s Int Int

{-
--Taken from "http://programmingpraxis.com/2012/07/10/sieving-for-totients/" NOT MY WORK
totients :: Integral a => a -> [a]
totients n = M.elems $ foldl (\m i -> if m M.! i == i then foldr (M.adjust (\x -> x*(i-1) `div` i)) m [i,2*i..n] else m)
	(M.fromList $ zip [0..n] [0..n]) [2..n]
-}

totients n = do
	arr <- newListArray (0,n) [0..]
	mapM_ (innerFold arr n) [2..n]
	getAssocs arr

totients' n = do
	arr <- newListArray (0,n) [0..]
	mapM_ (innerFold arr n) [2..n]
	return arr

innerFold arr n i = do
	a <- readArray arr i
	when (a == i) $
		mapM_ (adjust arr (\x -> x*(i-1) `div` i)) [i,2*i..n]

adjust :: TotientSieve s -> (Int -> Int) -> Int -> ST s ()
adjust arr f i = readArray arr i >>= writeArray arr i . f

minFun :: (Fractional a, Ord a) => (Int, a) -> (Int, Int) -> (Int, a)
minFun a b
	| ((uncurry $ on (/=) (sort . show)) b) || ((snd a) < b') = a
	| otherwise = (fst b, b')
	where b' = (uncurry ((/) `on` fromIntegral)) b

main = print $ fst $ minimumBy (comparing $ uncurry ((/) `on` fromIntegral)) $ filter (uncurry $ on (==) (sort . show)) $ drop 2 $ runST $ totients $ 10^7