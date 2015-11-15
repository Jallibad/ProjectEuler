import Data.List
import MathFunctions

abundants = filter (\x -> (sum $ init $ factors x) > x) [1..]

isAbundantSum n = any ((`elem` lessAbundants) . (n-)) lessAbundants
	where lessAbundants = takeWhile (<=n) abundants

main = print $ sum $ filter (not . isAbundantSum) [1..28123]