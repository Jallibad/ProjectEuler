isqrt :: Integral (a) => a -> a
isqrt 0 = 0
isqrt 1 = 1
isqrt n = head $ dropWhile (\x -> x*x > n) $ iterate (\x -> (x + n `div` x) `div` 2) (n `div` 2)

intToBinary :: Integral (a) => a -> [Char]
intToBinary n = reverse $ snd $ foldr (\digit (number, previous) -> if number>=digit then (number-digit, '1':previous) else (number, '0':previous)) (n, []) $ takeWhile (<=n) $ iterate (*2) 1

isPalindrome string = first == second
	where	half = (length string) `div` 2
		first = take half string
		second = take half $ reverse string

main = print $ sum $ filter (\x -> (isPalindrome $ show x) && (isPalindrome $ intToBinary x)) [1..10^6-1]