import Fraction
import Data.List (groupBy, sortBy)
import Data.Ord (comparing)
import Data.Function (on)
import Text.Printf (printf)

simplifyList :: Integral (a) => [(a, Fraction)] -> [(a, Fraction)]
simplifyList = map (foldr1 (\(v, p1) (_, p2) -> (v, p1+p2))) . groupBy ((==) `on` fst) . sortBy (comparing fst)

rollProbabilities options rolls = (iterate (\prob -> simplifyList [(v1+v2, p1*p2) | (v1, p1) <- prob, (v2, p2) <- options]) options) !! (rolls-1)

fourSided = zip [1..4] $ repeat (Fraction 1 4)
rolledNineTimes = rollProbabilities fourSided 9

sixSided = zip [1..6] $ repeat (Fraction 1 6)
rolledSixTimes = rollProbabilities sixSided 6

main = putStrLn $ printf "%.7f" answer
	where answer = value $ sum $ map (\(v, p) -> (sum $ map snd $ takeWhile ((<v) . fst) rolledSixTimes)*p) rolledNineTimes :: Double