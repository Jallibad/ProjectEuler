import Data.Function
import Data.List
import Data.Ratio
import qualified Data.Map as M
import MathFunctions
import Data.Ord (comparing)
import Data.Array.ST
import Control.Monad.ST
import Control.Monad

type TotientSieve s = STUArray s Int Int

{-
--Taken from "http://programmingpraxis.com/2012/07/10/sieving-for-totients/" NOT MY WORK
totients :: Integral a => a -> [a]
totients n = M.elems $ foldl (\m i -> if m M.! i == i then foldr (M.adjust (\x -> x*(i-1) `div` i)) m [i,2*i..n] else m)
	(M.fromList $ zip [0..n] [0..n]) [2..n]
-}

totients n = runST $ do
	arr <- newListArray (0,n) [0..]
	mapM_ (innerFold arr n) [2..n]
	getAssocs arr

innerFold arr n i = do
	a <- readArray arr i
	when (a == i) $
		mapM_ (adjust arr (\x -> x*(i-1) `div` i)) [i,2*i..n]

adjust :: TotientSieve s -> (Int -> Int) -> Int -> ST s ()
adjust arr f i = readArray arr i >>= writeArray arr i . f

main = print $ fst $ minimumBy (comparing $ uncurry ((/) `on` fromIntegral)) $ filter (uncurry $ on (==) (sort . show)) $ drop 2 $ totients $ 10^7