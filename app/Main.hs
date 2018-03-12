module Main where

import Tetris.Logic

main :: IO ()
main = do
    let board = populateBoard "Q0,Q2"
    putStrLn $ show board
