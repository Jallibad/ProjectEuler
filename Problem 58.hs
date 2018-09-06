import Control.Arrow ((***))
import PrimeFunctions (isPrime)

diagonals :: Integral a => [[a]]
diagonals = [1] : map part [3,5..]
	where part n = map (\x -> n^2-x*(n-1)) [3,2,1,0]

--diagonals' n
--	| n `mod` 4 == 1 = 

main = print $ fst $ head $ dropWhile (uncurry (>=) . snd) $ zip [3,5..] $ drop 2 $ scanl (flip updateFunc) (0, 0) diagonals

--updateFunc :: Integral a => (a, a) -> [a] -> (a, a)
updateFunc current = (+10*(length $ filter isPrime current)) *** (+(length current))