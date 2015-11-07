import Data.Char

series :: Int -> [a] -> [[a]]
series amount list
	| length list >= amount = (take amount list):(series amount $ tail list)
	| otherwise = []

main = do
	file <- readFile "Problem 8 Series.txt"
	print $ maximum $ map (product . map digitToInt) $ series 13 file