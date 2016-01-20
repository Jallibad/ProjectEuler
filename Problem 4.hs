import ListFunctions (isPalindrome)


main = print $ maximum $ filter (isPalindrome . show) [a*b | a <- [999,998..10], b <- [999,998..a]]