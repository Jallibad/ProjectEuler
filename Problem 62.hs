import Data.List
import Data.Function
import Data.Ord

cubes = groupBy ((==) `on` (length . snd)) $ map (\x -> (x, sort $ show $ x^3)) [1..]

f n = filter ((==) 5 . snd) $ map (\x -> (fst $ head x, length x)) $ groupBy ((==) `on` snd) $ sortBy (comparing snd) n

main = print $ (fst $ head $ concatMap f cubes)^3