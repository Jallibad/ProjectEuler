reverseNumber :: (Show a, Integral a) => Read (a) => a -> a
reverseNumber number = read $ reverse $ show number

isPalindrome :: (Show a, Integral a) => a -> Bool
isPalindrome number = take (numberLength `div` 2) numberString == reverse (if (numberLength `rem` 2 == 0) then secondHalf else tail secondHalf)
	where	numberString = show number
		numberLength = length numberString
		secondHalf = drop (numberLength `div` 2) numberString

lychrel :: Integer -> Bool
lychrel number = lychrel' number 50
	where	lychrel' _ 1 = True
		lychrel' number starting
			| isPalindrome changedNumber = False
			| otherwise = lychrel' changedNumber (starting-1)
			where changedNumber = number + reverseNumber number

main = print $ length $ filter lychrel [1..10^4]