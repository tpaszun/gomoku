module Main where

import ExampleBoards

import Gomoku.BitBoard
import Gomoku.BitBoard.Helpers
import Gomoku.BitBoard.Wizardry
import Gomoku.Abstractions

import Data.List

diagL size x y = [(startX + d, startY + d) | let startX = if x < y then 0                 else x - y,
                                             let startY = if x < y then y - x             else 0,
                                             let len    = if x < y then size - startY - 1 else size - startX - 1,
                                             d <- [0..len] ]

diagR size x y = [(startX - d, startY + d) | let startX = if x + y < (size - 1) then x + y      else size - 1,
                                             let startY = if x + y < (size - 1) then 0          else x + y - size + 1,
                                             let len    = if x + y < (size - 1) then startX     else size - startY - 1,
                                             d <- [0..len] ]

getRDiag = getDiag rightDiagonal
getLDiag = getDiag leftDiagonal

getDiag f board (x,y) =
  show $ lineToFieldList (fst diag) (snd diag)
  where
    diag = f exampleBoard5 (Move x y Black)


main :: IO ()
main = do
    printBoardDiagonals exampleBoard5

printBoardDiagonals board = do
    putStrLn $ showBoardAsBinary board
    let size = boardLength board
    let allMoves = [(x,y) | x <- [0..size-1], y <- [0..size-1]]

    let leftDiagonals = fmap (getLDiag board) allMoves
    putStrLn "\nleft diagonals"
    mapM_ (putStrLn) leftDiagonals

    let rightDiagonals = fmap (getRDiag board) allMoves
    putStrLn "\nright diagonals"
    mapM_ (putStrLn) rightDiagonals



