import Data.Maybe
import Data.List
import Data.Array
import Data.Char
import Debug.Trace
import System.IO
debug = flip trace

-- TYPES AND CONSTANTS

type SquareIdx = Int
type Val = Int
type Square = Maybe Int
type Unit = [Square]
type Move = (SquareIdx, Val)
type Board = Array Int Square


boxIdxs = [ [0, 1, 2, 9, 10, 11, 18, 19, 20]
          , [3, 4, 5, 12, 13, 14, 21, 22, 23]
          , [6, 7, 8, 15, 16, 17, 24, 25, 26] 
          , [27, 28, 29, 36, 37, 38, 45, 46, 47]
          , [30, 31, 32, 39, 40, 41, 48, 49, 50]
          , [33, 34, 35, 42, 43, 44, 51, 52, 53]
          , [54, 55, 56, 63, 64, 65, 72, 73, 74]
          , [57, 58, 59, 66, 67, 68, 75, 76, 77]
          , [60, 61, 62, 69, 70, 71, 78, 79, 80] ]

vals = [1..9]
unitSize = 9
boardSize = 80
squareIdxs = [0..boardSize]

-- PREDEFINED INPUT

inputBoardList = [Nothing, Nothing, Nothing, Just 2, Just 6, Nothing, Just 7, Nothing, Just 1, Just 6, Just 8, Nothing, Nothing, Just 7, Nothing, Nothing, Just 9, Nothing,Just 1, Just 9, Nothing, Nothing, Nothing, Just 4, Just 5, Nothing, Nothing,Just 8, Just 2, Nothing, Just 1, Nothing, Nothing, Nothing, Just 4, Nothing,Nothing, Nothing, Just 4, Just 6, Nothing, Just 2, Just 9, Nothing, Nothing,Nothing, Just 5, Nothing, Nothing, Nothing, Just 3, Nothing, Just 2, Just 8,Nothing, Nothing, Just 9, Just 3, Nothing, Nothing, Nothing, Just 7, Just 4,Nothing, Just 4, Nothing, Nothing, Just 5, Nothing, Nothing, Just 3, Just 6,Just 7, Nothing, Just 3, Nothing, Just 1, Just 8, Nothing, Nothing, Nothing]
inputBoard = (arrayFromList inputBoardList) :: Board

-- UTILITY

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n l
  | n > 0 = (take n l) : (chunk n (drop n l))
  | otherwise = error "Negative n"

every :: Int -> [a] -> [a]
every n = iter 0
  where iter _ [] = []
        iter 0 (x:xs) = x:(iter 1 xs)
        iter i (x:xs) = iter ((i+1) `mod` n) xs

intersperseN :: Int -> a -> [a] -> [a]
intersperseN n elem = iter (n-1)
  where iter _ [] = []
        iter _ (x:[]) = [x]
        iter 0 (x:y:xs) = x:elem:(iter (n-1) (y:xs))
        iter i (x:y:xs) = x:(iter ((i-1) `mod` n) (y:xs))

arrayFromList :: [a] -> Array Int a
arrayFromList l = array (0, len - 1) [(i, l !! i) | i <- [0..((length l) - 1)]]
  where len = length l

existant :: [Maybe a] -> [a]
existant = map fromJust . filter isJust

-- BOARD HELPERS

getSquare :: Board -> SquareIdx -> Square
getSquare = (!)

getSquares :: Board -> [SquareIdx] -> [Square]
getSquares board = map (getSquare board)

getRow :: Board -> SquareIdx -> Unit
getRow board i = getSquares board idxs
  where row = div i unitSize
        idxs = take unitSize . drop (unitSize * row) $ squareIdxs

getCol :: Board -> SquareIdx -> Unit
getCol board i = getSquares board idxs
  where idxs = every unitSize $ drop (mod i unitSize) squareIdxs

getBox :: Board -> SquareIdx -> Unit
getBox board i = getSquares board idxs
  where idxs = head . filter (elem i) $ boxIdxs

getUnits :: Board -> SquareIdx -> [Unit]
getUnits board i = [getRow board i, getCol board i, getBox board i]

-- SOLUTION TIME

isSolved :: Board -> Bool
isSolved board = all isJust $ getSquares board squareIdxs

getFreeSquareIdxs :: Board -> [SquareIdx]
getFreeSquareIdxs board = filter (isNothing . getSquare board) squareIdxs

getLegalMovesForIdx :: Board -> SquareIdx -> [Move]
getLegalMovesForIdx board idx = 
  let units = getUnits board idx
      neighborVals = nub . existant . concat $ units
      valIsLegalMove = (`notElem` neighborVals)
  in map (\x -> (idx, x)) . filter valIsLegalMove $ vals

getCorrectMoves :: Board -> [Move]
getCorrectMoves board =
  let free = getFreeSquareIdxs board
      moves = map (getLegalMovesForIdx board) free
      correctMoves = filter ((==1) . length) moves
  in concat correctMoves

makeMove :: Board -> (SquareIdx, Val) -> Board
makeMove board (idx, val) = board // [(idx, Just val)]

solve :: Board -> Board
solve board
  | isSolved board = board
  | length correctMoves > 0 = solve $ makeMove board (head correctMoves)
  | otherwise = error "Could not solve board"
  where correctMoves = getCorrectMoves board

-- SHOW YOUR WORK

rowToLine :: [Square] -> String
rowToLine =
  let getChar Nothing = '.'
      getChar (Just x) = head $ show x
  in intersperseN 3 '|' . map getChar

printBoard :: Board -> IO ()
printBoard board = do 
  putStrLn ""
  let rows = chunk unitSize squareIdxs
      vals = map rowToLine . map (getSquares board) $ rows
      lines = intersperseN 3 "---+---+---" vals
  mapM_ putStrLn lines
  putStrLn ""

-- RELEASE THE PROBLEM
parsePuzzle :: String -> Board
parsePuzzle input =
  let vals = concat $ lines input
      toSquare '.' = Nothing
      toSquare x = Just (digitToInt x)
      list = map toSquare vals 
  in (arrayFromList list) :: Board

main = do
  contents <- readFile "data.txt"
  let puzzle = parsePuzzle contents
  putStrLn "Input:"
  printBoard puzzle
  let solution = solve puzzle
  putStrLn "Solution:"
  printBoard solution


