import MathFunctions (isqrt)
import Control.Applicative ((<$>), (<*>))
import Data.List (intersperse)

correct :: [Char] -> Bool
correct ('1':_:'2':_:'3':_:'4':_:'5':_:'6':_:'7':_:'8':_:'9':_:'0':[]) = True
correct _ = False

lowerBound = isqrt $ read $ intersperse '0' (['1'..'9']++"0") :: Integer
upperBound = isqrt $ read $ intersperse '9' (['1'..'9']++"0") :: Integer

removeTwoDigits n = (n `div` 100)*100

main = print $ head $ filter (correct . show . (^2)) $ (+) <$> [removeTwoDigits lowerBound, (removeTwoDigits lowerBound)+100.. removeTwoDigits upperBound] <*> [30, 70]