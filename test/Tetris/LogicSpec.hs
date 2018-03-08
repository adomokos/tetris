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

sortNub :: (Ord a) => [a] -> [a]
sortNub = map head . group . sort

q :: Cell -> Shape
q (r,c) = [(r,c),(r,c+1),(r+1,c+1),(r+1,c)]

z :: Cell -> Shape
z (r,c) = [(r,c),(r,c+1),(r-1,c+1),(r-1,c+2)]

s :: Cell -> Shape
s (r,c) = [(r,c),(r,c+1),(r+1,c+1),(r+1,c+2)]

t :: Cell -> Shape
t (r,c) = [(r,c),(r,c+1),(r,c+2),(r-1,c+1)]

i :: Cell -> Shape
i (r,c) = [(r,c),(r,c+1),(r,c+2),(r,c+3)]

l :: Cell -> Shape
l (r,c) = [(r,c),(r,c+1),(r+1,c),(r+2,c)]

j :: Cell -> Shape
j (r,c) = [(r,c),(r,c+1),(r+1,c+1),(r+2,c+1)]

drawShape :: Ord b => t -> (t -> [b]) -> [b]
drawShape cell shape = sort $ shape cell

addShapeToBoard
    :: Cell -> (Cell -> [Cell]) -> Board -> Board
addShapeToBoard cell@(r,c) shape board =
    let result = sortNub $ drawShape cell shape ++ board
     in if (length result `mod` 4 > 0)
           then addShapeToBoard (r+1,c) shape board
           else result

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    describe "Tetris Logic" $ do
        it "can position elements" $ do
            drawShape (0,0) q `shouldBe` [(0,0), (0,1), (1,0), (1,1)]
            drawShape (1,0) z `shouldBe` [(0,1), (0,2), (1,0), (1,1)]
            drawShape (0,0) s `shouldBe` [(0,0), (0,1), (1,1), (1,2)]
            drawShape (1,0) t `shouldBe` [(0,1), (1,0), (1,1), (1,2)]
            drawShape (0,0) i `shouldBe` [(0,0), (0,1), (0,2), (0,3)]
            drawShape (0,1) l `shouldBe` [(0,1), (0,2), (1,1), (2,1)]
            drawShape (0,2) j `shouldBe` [(0,2), (0,3), (1,3), (2,3)]
        it "can position two blocks adjacent to each other" $ do
            let board = addShapeToBoard (0,2) q $ addShapeToBoard (0,0) q []
            board `shouldBe` [(0,0), (0,1), (0,2), (0,3),
                              (1,0), (1,1), (1,2), (1,3)]
            let board = addShapeToBoard (0,8) q $
                        addShapeToBoard (0,4) i $
                        addShapeToBoard (0,0) i []
            board `shouldBe` [(0,0), (0,1), (0,2), (0,3),
                              (0,4), (0,5), (0,6), (0,7),
                              (0,8), (0,9), (1,8), (1,9)]
        it "can position two block on top of each other" $ do
            let board = addShapeToBoard (0,1) q $ addShapeToBoard (0,0) q []
            board `shouldBe` [(0,0), (0,1), (1,0), (1,1),
                              (2,1), (2,2), (3,1), (3,2)]
            let board = addShapeToBoard (0,1) i $ addShapeToBoard (0,0) i []
            board `shouldBe` [(0,0), (0,1), (0,2), (0,3),
                              (1,1), (1,2), (1,3), (1,4)]


