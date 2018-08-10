{-# LANGUAGE ViewPatterns #-}

import Control.Arrow ((&&&))
import ListFunctions (isPalindrome)

reverseNumber :: (Show a, Integral a, Read a) => a -> a
reverseNumber = uncurry (+) . (id &&& read . reverse . show)

lychrel :: Integer -> Bool
lychrel number = lychrel' number 50
	where
		lychrel' _ 1 = True
		lychrel' (reverseNumber -> number) starting =
			(not $ isPalindrome $ show number) && (lychrel' number (starting-1))

main = print $ length $ filter lychrel [1..10^4]