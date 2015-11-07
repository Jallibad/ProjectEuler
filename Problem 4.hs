import Data.List

isPalindrome number = first == second
	where	stringForm = show number
		half = (length stringForm) `div` 2
		first = take half stringForm
		second = take half $ reverse stringForm

main = print $ last $ sort $ filter isPalindrome [a*b | a <- [1..999], b <- [1..999]]