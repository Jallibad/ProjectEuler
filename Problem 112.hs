import Data.List

isBouncy :: Show (a) => Integral (a) => a -> Bool
isBouncy n = all (/= sort s) [s, reverse s]
	where s = show n

main = print $ snd $ head $ dropWhile (\(a, b) -> a < 0.99*(fromIntegral b)) $ zip [1..] $ filter isBouncy [1..]