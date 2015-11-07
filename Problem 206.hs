import Data.List
import Control.Applicative

isqrt :: Integral (a) => a -> a
isqrt 0 = 0
isqrt 1 = 1
isqrt n = head $ dropWhile (\x -> x*x > n) $ iterate (\x -> (x + n `div` x) `div` 2) (n `div` 2)

correct :: [Char] -> Bool
correct ('1':_:'2':_:'3':_:'4':_:'5':_:'6':_:'7':_:'8':_:'9':_:'0':[]) = True
correct _ = False

lowerBound = isqrt $ read $ intersperse '0' (['1'..'9']++"0") :: Integer
upperBound = isqrt $ read $ intersperse '9' (['1'..'9']++"0") :: Integer

main = print $ head $ filter (correct . show . (^2)) $ (+) <$> [(lowerBound `div` 100)*100, (lowerBound `div` 100)*100+100.. (upperBound `div` 100)*100] <*> [30, 70]