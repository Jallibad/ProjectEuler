import ListFunctions (isPalindrome)
import MathFunctions (intToBinary)

main = print $ sum $ filter (\x -> (isPalindrome $ show x) && (isPalindrome $ intToBinary x)) options
	where options = [1,3..10^6-1] :: [Int]