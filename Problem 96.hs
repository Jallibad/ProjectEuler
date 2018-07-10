{-# LANGUAGE TupleSections, TemplateHaskell, ParallelListComp #-}

import Control.Applicative
import Control.Lens
import Control.Lens.Setter
import Data.Array
import Data.Bifunctor
import Data.Char
import Data.Either
import Data.Foldable (foldr', find)
import Data.List.Split
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

data GridSquare = Completed {_completed :: Int} | Uncompleted {_uncompleted :: Set.Set Int}

makeLenses ''GridSquare

--instance Bifunctor GridSquare 

constructGridSquare :: Set.Set Int -> GridSquare
constructGridSquare set
	| Set.size set == 1 = Completed $ Set.findMax set
	| otherwise = Uncompleted set

peFormatToGridSquare :: Char -> GridSquare
peFormatToGridSquare '0' = constructGridSquare fullyUnknown
peFormatToGridSquare x = Completed $ digitToInt x

removeOption :: GridSquare -> Int -> GridSquare
removeOption (Uncompleted set) n = constructGridSquare $ Set.delete n set
removeOption x _ = x

options :: GridSquare -> Int
options (Completed _) = 0
options (Uncompleted set) = Set.size set - 1

instance Show GridSquare where
	show (Completed g) = show g
	show _ = "0"

newtype Sudoku = Sudoku (Array (Int, Int) GridSquare)

instance Show Sudoku where
	show (Sudoku grid) = unlines $ chunksOf 9 $ map (head . show) $ elems grid

remainingUnknowns :: Sudoku -> Int
remainingUnknowns (Sudoku grid) = foldr' ((+) . options) 0 grid

fullyUnknown = Set.fromDistinctAscList [1..9]

readGrid :: String -> Sudoku
readGrid = Sudoku . fmap peFormatToGridSquare . listArray ((0, 0), (8, 8))

readGrids :: [String] -> Map.Map String Sudoku
readGrids [] = Map.empty
readGrids s = Map.insert title (readGrid $ concat x) $ readGrids xs
	where (title:x,xs) = splitAt 10 s

grid1 = readGrid "003020600900305001001806400008102900700000008006708200002609500800203009005010300"
grid2 = readGrid "200080300060070084030500209000105408000000000402706000301007040720040060004010003"

removeLinesAndGrids :: Sudoku -> Sudoku
removeLinesAndGrids (Sudoku grid) = Sudoku $ accum removeOption grid $ concat rowsToUpdate
	where rowsToUpdate = [(fillRow a)++(fillColumn a)++(fillGrid a) | ((x,y), Completed e) <- assocs grid, let a=((x,y),e)]

--completeLoneNumberLinesAndGrids :: Sudoku -> Sudoku
--completeLoneNumberLinesAndGrids
--findLoner
findLoners y = Map.keysSet . Map.filter (== Just 1) . Map.unionsWith (liftA2 (+)) . map toMap . view (rowLens y)
setIf :: Set.Set Int -> GridSquare -> GridSquare
setIf x s = fromMaybe s $ fmap Completed $ find (const True) $ Set.intersection x $ s^.uncompleted

toMap (Completed x) = Map.adjust (const Nothing) x $ Map.fromAscList $ zip [1..9] $ repeat $ Just 0
toMap (Uncompleted set) = Map.fromSet (const $ Just 1) set

fix y grid lens = over (lens y) (map $ setIf $ findLoners y grid) grid

--type GridGetter = Setter' Sudoku GridSquare

--fullRow :: (Int, Int) -> Lens' Sudoku [GridSquare]
--fullRow (x,y) = map () [0..8]

getGridSquares :: [(Int, Int)] -> Sudoku -> [GridSquare]
getGridSquares ix (Sudoku board) = map (board !) ix

getRow (_,y) = map (,y) [0..8]
getColumn (x,_) = map (x,) [0..8]
getGrid (x,y) = [(xs,ys) | xs <- [lowX.. lowX+2], ys <- [lowY.. lowY+2]]
	where
		lowX = x-(x `mod` 3)
		lowY = y-(y `mod` 3)

--sudokuLens :: (Sudoku -> GridSquare -> Sudoku) -> Lens' Sudoku GridSquare
sudokuLens :: (Functor f) => (Int, Int) -> (GridSquare -> f GridSquare) -> (Sudoku -> f Sudoku)
sudokuLens i = lens getter const--setter
	where
		getter (Sudoku board) = board ! i
		--setter (Sudoku board) gridSquare = 

singleLens i = lens getter setter
	where
		getter (Sudoku grid) = grid ! i
		setter (Sudoku grid) v = Sudoku $ grid // [(i, v)]

rowLens y = lens getter setter
	where
		getter (Sudoku grid) = elems $ ixmap ((0,y),(8,y)) id $ grid
		setter (Sudoku grid) vs = Sudoku $ grid // [((x,y),v) | x <- [0..8] | v <- vs]

columnLens x = lens getter setter
	where
		getter (Sudoku grid) = elems $ ixmap ((x,0),(x,8)) id $ grid
		setter (Sudoku grid) vs = Sudoku $ grid // [((x,y),v) | y <- [0..8] | v <- vs]

fillRow ((x,y),a) = map ((,a) . (,y)) [0..8]
fillColumn ((x,y),a) = map ((,a) . (x,)) [0..8]
fillGrid ((x,y),a) = [((xs,ys),a) | xs <- [lowX.. lowX+2], ys <- [lowY.. lowY+2], xs/=x || ys/=y]
	where	lowX = (x `div` 3)*3
		lowY = (y `div` 3)*3

solvePuzzle :: Sudoku -> Sudoku
solvePuzzle = head . dropWhile ((/=0) . remainingUnknowns) . iterate (foldl1 (.) [removeLinesAndGrids])

main = readFile "Problem 96 sudoku.txt" >>= print . fmap solvePuzzle . readGrids . lines