import ListFunctions (isPalindrome)

main = print $ maximum $ concatMap (\a -> take 1 $ filter (isPalindrome . show) $ map (a*) [999,998..a]) [999,998..10]