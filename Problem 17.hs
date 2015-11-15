import Data.Char

numberToWordsLength :: Int -> Int
numberToWordsLength number
	| number < 20 = teenLengths !! number
	| number < 100 && number `mod` 10 == 0 = twoDigitLengths !! ((digitToInt $ head string)-2)
	| number < 100 = (numberToWordsLength $ (digitToInt $ head string)*10)+(numberToWordsLength $ digitToInt $ last string)
	| number < 1000 && number `mod` 100 == 0 = (numberToWordsLength $ digitToInt $ head string)+7
	| number < 1000 = (numberToWordsLength $ (digitToInt $ head string)*100)+3+(numberToWordsLength $ read $ tail string)
	| number == 1000 = 11
	where	string = show number
		teenLengths = map length ["zero","one","two","three","four","five","six","seven","eight","nine","ten","eleven","twelve","thirteen","fourteen","fifteen","sixteen","seventeen","eighteen","nineteen"]
		twoDigitLengths = map length ["twenty","thirty","forty","fifty","sixty","seventy","eighty","ninety"]

main = print $ sum $ map numberToWordsLength [1..1000]