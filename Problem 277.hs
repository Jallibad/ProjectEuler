import Data.List
import ListFunctions

f [] x = x
f ('D':xs) x = f xs $ (3*x)
f ('U':xs) x = f xs $ (3*x-2) `div` 4
f ('d':xs) x = f xs $ (3*x+1) `div` 2
f _ _ = error "Wrong step"

makeSteps 1 = []
makeSteps n = case n `mod` 3 of
		0 -> 'D' : (makeSteps $ n `div` 3)
		1 -> 'U' : (makeSteps $ (4*n+2) `div` 3)
		2 -> 'd' : (makeSteps $ (2*n-1) `div` 3)

main = print $ head $ filter (isPrefixOf seq . makeSteps) $ map (f $ reverse seq) [20371465, 20371465+3..]
	where seq = "UDDDUdddDDUDDddDdDddDDUDDdUUDd"