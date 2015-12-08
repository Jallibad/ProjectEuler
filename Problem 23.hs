import Data.List
import qualified Data.Map as Map
import MathFunctions

abundants = filter (\x -> (sum $ init $ factors x) > x) [1..28123-12]

abundantMap = Map.fromDistinctAscList $ zip abundants $ repeat 0

isAbundantSum x = any (\i -> Map.member (x-i) abundantMap) $ takeWhile (<x) abundants

main = print $ sum $ filter (not . isAbundantSum) [1..28123]