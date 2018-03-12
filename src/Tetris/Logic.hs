module Tetris.Logic
    (
       Row, Column, Cell, Shape, Board,
       q, z, s, t, i, l, j,
       drawShape, placeShapeOnBoard, height,
       parseInput, populateBoard
    ) where

import Prelude hiding (Left, Right)
import qualified Data.Map as M
import Data.List
import qualified Data.List.Split as Split
import Data.Char

type Row = Int
type Column = Int
type Cell = (Row, Column)
type Shape = Cell -> [Cell]
type Board = [Cell]

q :: Shape
q (r,c) = [(r,c),(r,c+1),(r+1,c+1),(r+1,c)]

z :: Shape
z (r,c) = [(r+1,c),(r+1,c+1),(r,c+1),(r,c+2)]

s :: Shape
s (r,c) = [(r,c),(r,c+1),(r+1,c+1),(r+1,c+2)]

t :: Shape
t (r,c) = [(r+1,c),(r+1,c+1),(r+1,c+2),(r,c+1)]

i :: Shape
i (r,c) = [(r,c),(r,c+1),(r,c+2),(r,c+3)]

l :: Shape
l (r,c) = [(r,c),(r,c+1),(r+1,c),(r+2,c)]

j :: Shape
j (r,c) = [(r,c),(r,c+1),(r+1,c+1),(r+2,c+1)]

charToShape :: Char -> Shape
charToShape c =
    M.fromList [('q',q),('z',z),('s',s),('t',t),
                ('i',i),('l',l),('j',j)] M.! c

parseInput :: String -> [(Shape,Column)]
parseInput = map (\x -> ((charToShape . toLower . head) x,
                          read (tail x) :: Int))
                 . Split.splitOn ","

sortNub :: (Ord a) => [a] -> [a]
sortNub = map head . group . sort

height :: Board -> Int
height [] = 0
height board = (succ . fst . last) board

drawShape :: Shape -> Cell -> [Cell]
drawShape shape = sort . shape

addShapeToBoard :: Shape -> Cell -> [Cell] -> Board
addShapeToBoard shape cell [] = drawShape shape cell ++ []
addShapeToBoard shape (-1,c) board = drawShape shape (0, c) ++ board
addShapeToBoard shape cell@(r,c) board =
    let shapeCells = drawShape shape cell
        noOverlappingCells = null (shapeCells `intersect` board)
     in if noOverlappingCells
        then addShapeToBoard shape (r-1,c) board
        else drawShape shape (r+1,c) ++ board

collapseFullRows :: Board -> Board
collapseFullRows =
    let formRows = groupBy (\r1 r2 -> fst r1 == fst r2)
        filterFullRows = filter (\x -> length x /= 10)
        renumberRows rows = zipWith (\x y -> [(y,c) | (_,c) <- x]) rows [0..]
        flatten = intercalate []
     in flatten . renumberRows . filterFullRows . formRows

placeShapeOnBoard :: Shape -> Column -> [Cell] -> Board
placeShapeOnBoard shape c =
    let updateBoard board = sortNub $ addShapeToBoard shape (height board, c) board
     in collapseFullRows . updateBoard

populateBoard :: String -> Board
populateBoard input =
    foldl (\acc (s,c) -> placeShapeOnBoard s c acc)
          []
          (parseInput input)
