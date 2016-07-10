import Data.List
import Data.Maybe
import Data.Function
import Debug.Trace

data Value = C2 | C3 | C4 | C5 | C6 | C7 | C8 | C9 | C10 | J | Q | K | A
	deriving (Eq, Ord, Bounded, Enum)

instance Show Value where
	show J = "J"
	show Q = "Q"
	show K = "K"
	show A = "A"
	show C10 = "T"
	show value = show $ (fromJust $ elemIndex value [(C2)..C9])+2

readValue :: String -> Value
readValue value = fst $ fromJust $ find ((==) value . snd) $ zip values $ map show values
	where
		values = [(minBound :: Value)..]

data Suit = H | D | C | S
	deriving (Eq, Show, Read, Enum)

data Card = Card {value :: Value, suit :: Suit}
	deriving (Eq)

instance Ord Card where
	compare = compare `on` value

readCard :: String -> Card
readCard string = Card (readValue value) (read suit)
	where (value, suit) = splitAt 1 string

instance Show Card where
	show (Card value suit) = (show value)++(show suit)

type Hand = [Card]

handValues :: Hand -> [Value]
handValues hand = map value hand

royalFlush :: Hand -> Bool
royalFlush hand = null $ [C10,J,Q,K,A] \\ (handValues hand)

straightFlush :: Hand -> Bool
straightFlush hand = null $ shouldBe \\ hand
	where
		(Card value1 suit1) = minimum hand
		shouldBe = zipWith Card [value1..] (replicate 5 suit1)

fourOfAKind :: Hand -> Bool
fourOfAKind hand = any ((==) 4 . length) $ group $ sort hand

fullHouse :: Hand -> Bool
fullHouse hand = [2,3] == (sort $ map length $ group $ sort $ map value hand)

flush :: Hand -> Bool
flush hand = all ((==) suit1 . suit) hand
	where suit1 = suit $ head hand

straight :: Hand -> Bool
straight hand = (take 5 [head sorted..]) == sorted
	where sorted = sort $ map value hand

threeOfAKind :: Hand -> Bool
threeOfAKind hand = any ((==3) . length) $ group $ map value $ sort hand

twoPairs :: Hand -> Bool
twoPairs hand = (length $ filter (>=2) $ map length $ group $ sort $ map value hand) == 2

onePair :: Hand -> Bool
onePair hand = any ((==2) . length) $ group $ map value $ sort hand

pairRank :: Hand -> Value
pairRank = head . head . filter ((==2) . length) . group . sort . map value

valueCompare :: Hand -> Hand -> Ordering
valueCompare [] _ = EQ
valueCompare _ [] = EQ
valueCompare (x:xs) (y:ys)
	| x /= y = compare x y
	| otherwise = valueCompare xs ys

pairCompare :: Hand -> Hand -> Ordering
pairCompare h1 h2 = if p1 == p2 then valueCompare h1 h2 else compare p1 p2
	where	p1 = pairRank h1
		p2 = pairRank h2

--GT means h1 won
handCompare :: Hand -> Hand -> Ordering
handCompare h1 h2
	| royalFlush h1 = GT
	| royalFlush h2 = LT
	| straightFlush h1 = if straightFlush h2 then EQ else GT
	| straightFlush h2 = LT
	| fourOfAKind h1 = if fourOfAKind h2 then EQ else GT
	| fourOfAKind h2 = LT
	| fullHouse h1 = if fullHouse h2 then EQ else GT
	| fullHouse h2 = LT
	| flush h1 = if flush h2 then EQ else GT
	| flush h2 = LT
	| straight h1 = if straight h2 then EQ else GT
	| straight h2 = LT
	| threeOfAKind h1 = if threeOfAKind h2 then EQ else GT
	| threeOfAKind h2 = LT
	| twoPairs h1 = if twoPairs h2 then EQ else GT
	| twoPairs h2 = LT
	| onePair h1 = if onePair h2 then pairCompare h1 h2 else GT
	| onePair h2 = LT
	|otherwise = compare (maximum h1) (maximum h2)

main = do
	file <- readFile "Problem 54 Poker Hands.txt"
	print $ map (uncurry handCompare . splitAt 5 . map readCard . words) $ lines file