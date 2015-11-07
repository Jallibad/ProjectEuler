import Data.Function
import Data.List
import Data.Ratio
import qualified Data.Map as M

--Taken from "http://programmingpraxis.com/2012/07/10/sieving-for-totients/" NOT MY WORK
totients :: Integral a => a -> [a]
totients n = M.elems $ foldl (\m i -> if m M.! i == i
    then foldr (M.adjust (\x -> div (x*(i-1)) i)) m [i,2*i..n] else m)
    (M.fromList $ zip [0..n] [0..n]) [2..n]

main = print $ (sum $ totients (10^6))-1 --Ignore 0/1 and 1/1