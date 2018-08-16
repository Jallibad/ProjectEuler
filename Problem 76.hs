import Data.Function.Memoize (memoFix2)

countSums :: Integer -> Integer
countSums n = memoFix2 countSums' n $ n-1

countSums' :: (Integer -> Integer -> Integer) -> Integer -> Integer -> Integer
countSums' _ 0 _ = 1
countSums' f n m = sum $ map (\x -> f (n-x) $ min x $ n-x) [1.. min n m]

main = print $ countSums 100