module Tetris.LogicSpec where

import Prelude hiding (Left, Right)
import Test.Hspec
import Data.List
{- import Data.Set -}
{- import qualified Tetris.Logic as L -}

type Row = Int
type Column = Int
type Cell = (Row, Column)
type Shape = [Cell]
type Board = [Cell]

q :: Cell -> Shape
q (r,c) = [(r,c),(r,c+1),(r+1,c+1),(r+1,c)]

z :: Cell -> Shape
z (r,c) = [(r+1,c),(r+1,c+1),(r,c+1),(r,c+2)]

s :: Cell -> Shape
s (r,c) = [(r,c),(r,c+1),(r+1,c+1),(r+1,c+2)]

t :: Cell -> Shape
t (r,c) = [(r+1,c),(r+1,c+1),(r+1,c+2),(r,c+1)]

i :: Cell -> Shape
i (r,c) = [(r,c),(r,c+1),(r,c+2),(r,c+3)]

l :: Cell -> Shape
l (r,c) = [(r,c),(r,c+1),(r+1,c),(r+2,c)]

j :: Cell -> Shape
j (r,c) = [(r,c),(r,c+1),(r+1,c+1),(r+2,c+1)]

sortNub :: (Ord a) => [a] -> [a]
sortNub = map head . group . sort

drawShape :: Ord b => t -> (t -> [b]) -> [b]
drawShape cell shape = sort $ shape cell

addShapeToBoard
    :: Cell -> (Cell -> Shape) -> Board -> Board
addShapeToBoard cell shape [] = drawShape cell shape ++ []
addShapeToBoard (-1,c) shape board = drawShape (0, c) shape ++ board
addShapeToBoard cell@(r,c) shape board =
    let shapeCells = drawShape cell shape
        overlappingCellCount = length (shapeCells `intersect` board)
     in if overlappingCellCount == 0
           then addShapeToBoard (r-1,c) shape board
           else drawShape (r+1,c) shape ++ board

placeShapeOnBoard :: Column -> (Cell -> Shape) -> Board -> Board
placeShapeOnBoard c shape board =
    sortNub $ addShapeToBoard (height board, c) shape board

height :: Board -> Int
height [] = 0
height board = succ . fst . last $ board

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    describe "Tetris Logic" $ do
        it "can position elements" $ do
            drawShape (0,0) q `shouldBe` [(0,0), (0,1), (1,0), (1,1)]
            drawShape (0,0) z `shouldBe` [(0,1), (0,2), (1,0), (1,1)]
            drawShape (0,0) s `shouldBe` [(0,0), (0,1), (1,1), (1,2)]
            drawShape (0,0) t `shouldBe` [(0,1), (1,0), (1,1), (1,2)]
            drawShape (0,0) i `shouldBe` [(0,0), (0,1), (0,2), (0,3)]
            drawShape (0,1) l `shouldBe` [(0,1), (0,2), (1,1), (2,1)]
            drawShape (0,2) j `shouldBe` [(0,2), (0,3), (1,3), (2,3)]
        it "can add one shape" $ do
            let board = placeShapeOnBoard 0 q $ placeShapeOnBoard 0 q []
            board `shouldBe` [(0,0), (0,1), (1,0), (1,1),
                              (2,0), (2,1), (3,0), (3,1)]
        it "can position two blocks adjacent to each other" $ do
            let board = placeShapeOnBoard 2 q $ placeShapeOnBoard 0 q []
            board `shouldBe` [(0,0), (0,1), (0,2), (0,3),
                              (1,0), (1,1), (1,2), (1,3)]
            let board = placeShapeOnBoard 8 q $
                        placeShapeOnBoard 4 i $
                        placeShapeOnBoard 0 i []
            board `shouldBe` [(0,0), (0,1), (0,2), (0,3),
                              (0,4), (0,5), (0,6), (0,7),
                              (0,8), (0,9), (1,8), (1,9)]
        it "can position two block on top of each other" $ do
            let board = placeShapeOnBoard 1 q $ placeShapeOnBoard 0 q []
            board `shouldBe` [(0,0), (0,1), (1,0), (1,1),
                              (2,1), (2,2), (3,1), (3,2)]
            let board = placeShapeOnBoard 1 i $ placeShapeOnBoard 0 i []
            board `shouldBe` [(0,0), (0,1), (0,2), (0,3),
                              (1,1), (1,2), (1,3), (1,4)]
        it "can tell the height of elements on board" $ do
            let board = placeShapeOnBoard 1 q $ placeShapeOnBoard 0 q []
            height board `shouldBe` 4

            -- T1,Z3,I4”
            let board = placeShapeOnBoard 4 i $
                        placeShapeOnBoard 3 z $
                        placeShapeOnBoard 1 t []
            board `shouldBe` [(0,2), (1,1), (1,2), (1,3),
                              (1,4), (1,5), (2,3), (2,4),
                              (3,4), (3,5), (3,6), (3,7)]
