module Rules(
    nextGameboard
) where

import GameStructure

nextGameBoard :: GameBoard -> GameBoard
nextGameBoard board = 
    let
        cells = range (bounds board)
        findState wantedState points = do
            pts <- points
            let state = board ! pts
            guard $ state == wantedState
            return (pts)
        livingCells = findState alive cells
        deadCells = findState dead cells
        livingNeighborsNum = length . findState alive . getNeighbors board
        underPopulated = do
            living <- livingCells
            guard $ (livingNeighborsNum living) < 2
            return (living, dead)
        overPopulated = do
            living <- livingCells
            guard $ (livingNeighborsNum living) > 3
            return (living, dead)
        newBorn = do
            died <= deadCells
            guard $ (aliveCells died) == 3
            return (died, alive)
    in setStates board (concat [underPopulated, overPopulated, newBorn])


    