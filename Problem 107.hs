import Control.Arrow ((&&&))
import Data.List (delete, minimumBy)
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Data.Ord (comparing)

data Vertex = Vertex {name :: Int, edges :: Map.Map Int Int} deriving (Show)

instance Eq Vertex where
	(Vertex a _) == (Vertex b _) = a == b

mst ((Vertex n _):xs) = mst' [n] xs
	where	mst' minimal [] = 0
		mst' minimal full = val + (mst' (toAdd:minimal) (delete (Vertex toAdd Map.empty) full))
			where (toAdd, val) = minimumBy (comparing snd) [(name y, (edges y) Map.! x) | x <- minimal, y <- full, x `Map.member` (edges y)]

readVertices = map (uncurry Vertex) . zip [0..] . map (fmap read . Map.fromDistinctAscList . filter ((/="-") . snd) . zip [0..] . splitOn ",") . lines
sumEdges vs = (sum $ map (sum . Map.elems . edges) vs) `div` 2

main = readFile "Problem 107 Network.txt" >>= print . uncurry (-) . (sumEdges &&& mst) . readVertices