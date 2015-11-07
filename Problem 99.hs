import Data.Ord (comparing)
import Data.List (maximumBy)

expToLog :: String -> Double
expToLog string = (read y) * (log $ read x)
	where (x, _:y) = break (==',') string

main = do
	file <- readFile "Problem 99 Base Exp.txt"
	print $ snd $ maximumBy (comparing fst) $ zip (map expToLog $ lines file) [1..]