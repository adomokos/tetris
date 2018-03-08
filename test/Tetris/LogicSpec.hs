module Tetris.LogicSpec where

import Prelude hiding (Left, Right)
import Test.Hspec
import Data.List
{- import Data.Set -}
{- import qualified Tetris.Logic as L -}

{-
   The board is represented as a 10x10 matrix.
   The columns and rows are numbered from 0-9,
   the bottom left cell is represented as (0,0),
   the top right cell is represented as (9,9).
-}

data Position = Start | Left | Right | Up | Down deriving (Show, Eq)
type Shape = [Position]

type Row = Int
type Column = Int
type Cell = (Row, Column)
type Board = [Cell]

q :: Shape
q = [Start,Right,Up,Left]

z :: Shape
z = [Start,Right,Down,Right]

s :: Shape
s = [Start,Right,Up,Right]

t :: Shape
t = [Start,Right,Down,Up,Right]

i :: Shape
i = [Start,Right,Right,Right]

l :: Shape
l =[Start,Right,Left,Up,Up]

j :: Shape
j =[Start,Right,Up,Up]

placeOnBoard :: Shape -> Column -> Board -> Board
placeOnBoard shape column board = []

sortNub :: (Ord a) => [a] -> [a]
sortNub = map head . group . sort

drawShape :: Cell -> Shape -> Board -> Board
drawShape _ [] [] = []
drawShape cell (Start:xs) board = cell : drawShape cell xs board
drawShape (r,c) (Right:xs) board = (r,c+1) : drawShape (r,c+1) xs board
drawShape (r,c) (Up:xs) board = (r+1,c) : drawShape (r+1,c) xs board
drawShape (r,c) (Down:xs) board = (r-1,c) : drawShape (r-1,c) xs board
drawShape (r,c) (Left:xs) board = (r,c-1) : drawShape (r,c-1) xs board

-- sorting tuple
-- sortBy (comparing $ fst) . sortBy (comparing $ snd) $ a

-- remove dupes
-- map head . group . sort $ a

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    describe "Tetris Logic" $ do
        it "can position elements" $ do
            sortNub (drawShape (0,0) q []) `shouldBe` [(0,0), (0,1), (1,0), (1,1)]
            sortNub (drawShape (0,0) i []) `shouldBe` [(0,0), (0,1), (0,2), (0,3)]
            sortNub (drawShape (0,3) i []) `shouldBe` [(0,3), (0,4), (0,5), (0,6)]
            sortNub (drawShape (1,0) t []) `shouldBe` [(0,1), (1,0), (1,1), (1,2)]
            sortNub (drawShape (1,0) z []) `shouldBe` [(0,1), (0,2), (1,0), (1,1)]
            sortNub (drawShape (0,0) s []) `shouldBe` [(0,0), (0,1), (1,1), (1,2)]
            sortNub (drawShape (0,1) l []) `shouldBe` [(0,1), (0,2), (1,1), (2,1)]
            sortNub (drawShape (0,2) j []) `shouldBe` [(0,2), (0,3), (1,3), (2,3)]
