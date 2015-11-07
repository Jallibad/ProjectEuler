isqrt :: Integral (a) => a -> a
isqrt 0 = 0
isqrt 1 = 1
isqrt n = head $ dropWhile (\x -> x*x > n) $ iterate (\x -> (x + n `div` x) `div` 2) (n `div` 2)

countFactors :: Integral (a) => a -> Int
countFactors number
	| (isqrt number)^2 == number = numberFactors-1
	| otherwise = numberFactors
	where numberFactors = 2*(length $ filter ((==) 0 . rem number) [1..isqrt number])

triangleNumber :: Integral (a) => a -> a
triangleNumber index = sum [1..index]

main = print $ triangleNumber (until (\x -> countFactors (triangleNumber x) > 500) succ 1)