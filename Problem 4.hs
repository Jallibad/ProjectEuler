import Data.List (find)
import Data.Maybe (mapMaybe)
import ListFunctions (isPalindrome)

main = print $ maximum $ mapMaybe (\a -> find (isPalindrome . show) $ map (a*) [999,998..a]) [999,998..10]