module Tetris.LogicSpec where

import Test.Hspec
import Tetris.Logic

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
        it "parses string input into shapes and columns" $ do
            let board = populateBoard "Q0,Q1"
            board `shouldBe` [(0,0),(0,1),(1,0),(1,1),
                              (2,1),(2,2),(3,1),(3,2)]
            height board `shouldBe` 4
