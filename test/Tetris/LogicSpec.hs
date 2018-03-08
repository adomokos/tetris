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
drawShape cell shape = sortNub $ shape cell

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    describe "Tetris Logic" $
        it "can position elements" $ do
            drawShape (0,0) q `shouldBe` [(0,0), (0,1), (1,0), (1,1)]
            drawShape (1,0) z `shouldBe` [(0,1), (0,2), (1,0), (1,1)]
            drawShape (0,0) s `shouldBe` [(0,0), (0,1), (1,1), (1,2)]
            drawShape (1,0) t `shouldBe` [(0,1), (1,0), (1,1), (1,2)]
            drawShape (0,0) i `shouldBe` [(0,0), (0,1), (0,2), (0,3)]
            drawShape (0,1) l `shouldBe` [(0,1), (0,2), (1,1), (2,1)]
            drawShape (0,2) j `shouldBe` [(0,2), (0,3), (1,3), (2,3)]
