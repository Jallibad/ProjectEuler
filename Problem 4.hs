import Data.List
import ListFunctions


main = print $ maximum $ filter (isPalindrome . show) [a*b | a <- [100..999], b <- [100..999]]