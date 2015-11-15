module ListFunctions where

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ [] = []
takeUntil f (x:xs) = case (f x) of
				True -> [x]
				False -> x : takeUntil f xs

takeLast :: Int -> [a] -> [a]
takeLast number list = drop (length list - number) list

isPalindrome :: Eq (a) => [a] -> Bool
isPalindrome xs = first == second
	where	half = (length xs) `div` 2
		first = take half xs
		second = take half $ reverse xs

--Taken from Data.List.Ordered
orderedUnion :: Ord a => [a] -> [a] -> [a]
orderedUnion = loop
  where
     loop [] ys = ys
     loop xs [] = xs
     loop (x:xs) (y:ys)
       = case compare x y of
          LT -> x : loop xs (y:ys)
          EQ -> x : loop xs ys
          GT -> y : loop (x:xs) ys