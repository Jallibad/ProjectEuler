import Data.List
import qualified Data.Set as Set
import ListFunctions
import MathFunctions

upperBound = 10^8

squares = map (^2) [1..isqrt (upperBound `div` 2)]

main = print $ Set.foldr' (+) 0 $ Set.fromList $ concatMap (filter (isPalindrome . show) . takeWhile (<upperBound) . drop 1 . scanl1 (+)) $ tails squares