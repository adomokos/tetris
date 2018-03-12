module Main where

import Tetris.Logic
import Data.List

main :: IO ()
main = do
    input <- getContents
    let positions =  lines input
        result = map (height . populateBoard) positions
    putStrLn $ intercalate ("\n") (map (show) result)
