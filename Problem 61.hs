import ListFunctions
import qualified Data.Set as Set

fourDigits = takeWhile (<10^4) . dropWhile (<10^3)

triangles = fourDigits $ map (\n -> n*(n+1) `div` 2) [1..]
squares = fourDigits $ map (^2) [1..]
pentagonals = fourDigits $ map (\n -> n*(3*n-1) `div` 2) [1..]
hexagonals = fourDigits $ map (\n -> n*(2*n-1)) [1..]
heptagonals = fourDigits $ map (\n -> n*(5*n-3) `div` 2) [1..]
octagonals = fourDigits $ map (\n -> n*(3*n-2)) [1..]

figurates = Set.fromList [triangles, squares, pentagonals, hexagonals, heptagonals, octagonals]

--ordered by means highest to lowest nth number not highest to lowest number

isMatch :: Show (a) => a -> a -> Bool
isMatch x y = (takeLast 2 $ show x) == (take 2 $ show y)

f :: Show (a) => [[a]] -> [a] -> [[a]]
f xss ys = [xs ++ [y] | xs <- xss, let x = last xs, y <- ys, isMatch x y]

--fs :: Show (a) => [[a]] -> Set.Set [a] -> [[a]]
fs xss set = filter (\x -> isMatch (last x) (head x)) $ fs' xss set
	where	fs' xss set
			| Set.null set = xss
			| otherwise = concatMap (\i -> fs' (f xss $ Set.elemAt i set) $ Set.deleteAt i set) [0..Set.size set - 1]

main = print $ head $ map sum $ fs (map (\x -> [x]) triangles) $ Set.delete triangles figurates