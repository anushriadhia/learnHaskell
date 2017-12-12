module GameStructure(
    State (..),
    Point,
    GameBoard,
    initializeBoard,
    setStates,
    getStates,
    getNeighbors
) where

--are the import statements supposed to go in the module or in main file?
import Data.List
import Data.Array

data State = alive | dead deriving (Eg, Show)
type Point = (Int, Int)
type GameBoard = Array Point State

initializeBoard :: Int -> GameBoard
initializeBoard (dim) = 
    array dimension $ zip (range dimension) (repeat dead)
    where dimension = ((0,0), (dim-1, dim-1))

setStates :: GameBoard -> [(Point, State)] -> GameBoard
setStates = (//)

getStates :: GameBoard -> [Point] -> [State]
getStates gboard points = map (gboard!) points

getNeighbors :: GameBoard -> Point -> [Point]
getNeighbors board point@(row,col) = 
    filter (/= point) $ filter (inRange(bounds board)) [(nrow, ncol)| nrow <- [row-1..row+1], ncol <- [col-1..col+1]]