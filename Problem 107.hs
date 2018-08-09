{-# LANGUAGE TupleSections, ViewPatterns #-}

import Control.Arrow ((&&&), first)
import Data.Function (on)
import Data.List (delete, minimumBy)
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Ord (comparing)

data Vertex = Vertex {name :: Int, edges :: Map.Map Int Int} deriving (Show)

instance Eq Vertex where
	(==) = (==) `on` name

mst ((Vertex n _):xs) = mst' [n] xs
mst' minimal [] = 0
mst' minimal full = uncurry (+) $ first nextVal $ minimumBy (comparing snd) stuff
	where
		stuff = catMaybes [fmap (n,) $ Map.lookup x e | x <- minimal, Vertex n e <- full]
		nextVal v = mst' (v:minimal) $ delete (Vertex v Map.empty) full

readVertices :: String -> [Vertex]
readVertices = zipWith Vertex [0..] . map readLine . lines

readLine :: String -> Map.Map Int Int
readLine = fmap read . Map.fromDistinctAscList . filter ((/="-") . snd) . zip [0..] . splitOn ","

sumEdges = (`div` 2) . sum . map (sum . Map.elems . edges)

main = readFile "Problem 107 Network.txt" >>= print . uncurry (-) . (sumEdges &&& mst) . readVertices