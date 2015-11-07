import MathFunctions
import FigurateNumbers

countFactors :: Integral (a) => a -> Int
countFactors number
	| (isqrt number)^2 == number = numberFactors-1
	| otherwise = numberFactors
	where numberFactors = 2*(length $ filter ((==) 0 . rem number) [1..isqrt number])

main = print $ triangleNumber (until (\x -> countFactors (triangleNumber x) > 500) succ 1)