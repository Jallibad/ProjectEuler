import Control.Arrow ((&&&))
import Data.Function (on)
import Data.List (groupBy, sort, sortOn)
import Data.Ord (comparing)

cubes = groupBy ((==) `on` (length . snd)) $ map (id &&& (sort . show . (^3))) [1..]

f n = map (fst . head) $ filter ((==5) . length) $ groupBy ((==) `on` snd) $ sortOn snd n

main = print $ (head $ concatMap f cubes)^3