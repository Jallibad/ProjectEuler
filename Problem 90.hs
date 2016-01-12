import ListFunctions
import Data.List

squares = ["01","04","06","16","18","25","36","46"]

options = pick 6 $ ['0'..'9']

validCombination (as,bs) = null $ squares \\ [sort [a,b] | a <- map sixToNine as, b <- map sixToNine bs]
	where	sixToNine '9' = '6'
		sixToNine x = x

main = print $ length $ filter validCombination [(a,b) | a <- options, b <- takeWhile (<=a) options]