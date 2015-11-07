import Data.List
import Data.Maybe
import Data.Function

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
fourOfAKind hand = any ((==) 4 . length) $ groupBy ((==) `on` value) $ sort hand

flush :: Hand -> Bool
flush hand = all ((==) suit1 . suit) hand
	where suit1 = suit $ head hand

straight :: Hand -> Bool
straight hand = (take 5 [head sorted..]) == sorted
	where sorted = sort $ map value hand

threeOfAKind :: Hand -> Bool
threeOfAKind hand = any ((==) 3 . length) $ groupBy ((==) `on` value) $ sort hand

onePair :: Hand -> Bool
onePair hand = (length $ filter (>=2) $ map length $ group $ sort $ map value hand) == 1

twoPairs :: Hand -> Bool
twoPairs hand = (length $ filter (>=2) $ map length $ group $ sort $ map value hand) == 2

main = do
	file <- readFile "Problem 54 Poker Hands.txt"
	print $ head $ map (splitAt 5 . map readCard . words) $ lines file