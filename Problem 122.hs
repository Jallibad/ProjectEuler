import Debug.Trace
import Data.Bits (popCount)
import Data.List (minimumBy)
import Data.Maybe (fromJust)
import qualified Data.Map as Map
import Control.Applicative
import Fraction

efficient :: Int -> Int
efficient power = fromJust $ efficient' 0 ((truncate $ logBase 2 $ fromIntegral power)+(popCount power)-1) [1]
	where	efficient' total max options@(current:_)
			| total > max = Nothing
			| current == power  = Just total
			| otherwise = maybeListMinimum [efficient' (total+1) max (next:options) | x <- options, x <= power-current, let next = current+x]

--83 is wrong

maybeListMinimum :: [Maybe Int] -> Maybe Int
maybeListMinimum [] = Nothing
maybeListMinimum list = minimumBy f list
	where	f a b = case compare <$> a <*> b of
				Nothing -> LT
				Just result -> result

data Tree a = Empty | Branch a [Tree a] deriving (Show)

update :: Ord (a) => Ord (k) => k -> a -> Map.Map k a -> Map.Map k a
update = Map.insertWith (\x y -> minimum [x,y])

efficients :: Map.Map Int Int -> Int -> [Int] -> Map.Map Int Int
efficients answers upperBound options@(current:_) = foldl (\map x -> update x (length options) map) answers $ dropWhile (>upperBound) $ map (+current) options