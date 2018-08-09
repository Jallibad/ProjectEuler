import ListFunctions (isPalindrome)

reverseNumber :: (Show a, Integral a) => Read (a) => a -> a
reverseNumber number = read $ reverse $ show number

lychrel :: Integer -> Bool
lychrel number = lychrel' number 50
	where	lychrel' _ 1 = True
		lychrel' number starting
			| isPalindrome $ show changedNumber = False
			| otherwise = lychrel' changedNumber (starting-1)
			where changedNumber = number + reverseNumber number

main = print $ length $ filter lychrel [1..10^4]