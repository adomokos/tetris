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
{- type Shape' = Cell Cell Cell Cell -}
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

height :: Board -> Int
height [] = 0
height board = (succ . fst . last) board

drawShape :: (Cell -> Shape) -> Cell -> Shape
drawShape shape = sort . shape

addShapeToBoard :: (Cell -> Shape) -> Cell -> Board -> Board
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

placeShapeOnBoard :: (Cell -> Shape) -> Column -> Board -> Board
placeShapeOnBoard shape c =
    let updateBoard board = sortNub $ addShapeToBoard shape (height board, c) board
     in collapseFullRows . updateBoard

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    describe "Tetris Logic" $ do
        it "can position elements" $ do
            drawShape q (0,0) `shouldBe` [(0,0), (0,1), (1,0), (1,1)]
            drawShape z (0,0) `shouldBe` [(0,1), (0,2), (1,0), (1,1)]
            drawShape s (0,0) `shouldBe` [(0,0), (0,1), (1,1), (1,2)]
            drawShape t (0,0) `shouldBe` [(0,1), (1,0), (1,1), (1,2)]
            drawShape i (0,0) `shouldBe` [(0,0), (0,1), (0,2), (0,3)]
            drawShape l (0,1) `shouldBe` [(0,1), (0,2), (1,1), (2,1)]
            drawShape j (0,2) `shouldBe` [(0,2), (0,3), (1,3), (2,3)]
        it "can add one shape" $ do
            let board = placeShapeOnBoard q 0 $ placeShapeOnBoard q 0 []
            board `shouldBe` [(0,0), (0,1), (1,0), (1,1),
                              (2,0), (2,1), (3,0), (3,1)]
        it "can position two blocks adjacent to each other" $ do
            let board = placeShapeOnBoard q 2 $ placeShapeOnBoard q 0 []
            board `shouldBe` [(0,0), (0,1), (0,2), (0,3),
                              (1,0), (1,1), (1,2), (1,3)]
        it "can position two block on top of each other" $ do
            let board = placeShapeOnBoard q 1 $ placeShapeOnBoard q 0 []
            board `shouldBe` [(0,0), (0,1), (1,0), (1,1),
                              (2,1), (2,2), (3,1), (3,2)]
            let board = placeShapeOnBoard i 1 $ placeShapeOnBoard i 0 []
            board `shouldBe` [(0,0), (0,1), (0,2), (0,3),
                              (1,1), (1,2), (1,3), (1,4)]
        it "can tell the height of elements on board" $ do
            let board = placeShapeOnBoard q 1 $ placeShapeOnBoard q 0 []
            height board `shouldBe` 4
            -- T1,Z3,I4 example
            let board = placeShapeOnBoard i 4 $
                        placeShapeOnBoard z 3 $
                        placeShapeOnBoard t 1 []
            board `shouldBe` [(0,2), (1,1), (1,2), (1,3),
                              (1,4), (1,5), (2,3), (2,4),
                              (3,4), (3,5), (3,6), (3,7)]
            height board `shouldBe` 4
        it "collapses rows after putting in new shapes" $ do
            let board = placeShapeOnBoard i 6 $
                        placeShapeOnBoard i 2 $
                        placeShapeOnBoard q 0 []
            board `shouldBe` [(0,0), (0,1)]
            height board `shouldBe` 1
            let board = placeShapeOnBoard i 6 $
                        placeShapeOnBoard i 2 $
                        placeShapeOnBoard i 6 $
                        placeShapeOnBoard i 2 $
                        placeShapeOnBoard q 0 []
            board `shouldBe` []
            height board `shouldBe` 0
        it "collapses rows example" $ do
            -- Q0,I2,I6,I0,I6,I6,Q2,Q4
            let board = placeShapeOnBoard q 4 $
                        placeShapeOnBoard q 2 $
                        placeShapeOnBoard i 6 $
                        placeShapeOnBoard i 6 $
                        placeShapeOnBoard i 0 $
                        placeShapeOnBoard i 6 $
                        placeShapeOnBoard i 2 $
                        placeShapeOnBoard q 0 []
            board `shouldBe` [(0,0),(0,1),(0,4),(0,5),
                              (0,6),(0,7),(0,8),(0,9),
                              (1,2),(1,3),(2,2),(2,3)]
            height board `shouldBe` 3
