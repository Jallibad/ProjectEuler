import Data.Graph
import Data.Tree

combineLists xs@(x:_) (y:ys) = (x+y) : combineLists' xs ys
	where	combineLists' (x:[]) (y:[]) = [x+y]
		combineLists' (x1:x2:xs) (y:ys) = (max (x1+y) (x2+y)) : combineLists' (x2:xs) ys

main = readFile "Problem 67 Tree.txt" >>= print . maximum . foldl1 combineLists . map (map read . words) . lines