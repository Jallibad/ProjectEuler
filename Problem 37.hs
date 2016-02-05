import Data.List
import Control.Applicative
import MathFunctions

possibilities :: [Int]
possibilities = map read $ concat $ iterate (\x -> (++) <$> x <*> singlePrimes) singlePrimes
	where singlePrimes = ["1","2","3","5","7","9"]

main = print $ sum $ take 11 $ filter isTruncatablePrime possibilities