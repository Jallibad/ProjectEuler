import Data.List
import ListFunctions (takeLast)

isPandigital :: String -> Bool
isPandigital number = ['1'..'9'] == (sort $ number)

bothPandigital :: String -> Bool
bothPandigital x = all isPandigital [take 9 x, takeLast 9 x]

main = print $ (fst $ head $ until (bothPandigital . show . snd . head) tail $ zip [1..] fibs)-1
	where fibs = 0 : 1 : zipWith (+) fibs (tail fibs)