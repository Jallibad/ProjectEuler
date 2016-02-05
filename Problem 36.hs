import ListFunctions (isPalindrome)
import Data.Bits

intToBinary :: Integral (a) => Bits (a) => a -> [Bool]
intToBinary n = map (testBit n) [0..truncate $ logBase 2 $ fromIntegral n]

main = print $ sum $ filter (\x -> (isPalindrome $ show x) && (isPalindrome $ intToBinary x)) options
	where options = [1,3..10^6-1] :: [Int]