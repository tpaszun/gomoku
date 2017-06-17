module Gomoku.BitBoard (
    BitBoard(..),
    getLineLength,
    CBitBoard,
    horizontal,
    vertical,
    diagonalL,
    diagonalR,
    getField
) where

import Gomoku.Abstractions

import qualified Data.Vector.Unboxed as U
import Data.Word

data BitBoard = BitBoard {
  internalRep :: U.Vector Word64,
  boardLength :: Int
}


-- get line length from index on bitboard
-- horizontal and vertical have length of board size
-- diagonals have length from 1 to board size
getLineLength :: BitBoard -> Int -> Int
getLineLength (BitBoard _ boardSize) lineNum
  | lineNum < boardSize * 2 = boardSize
  | diagonalLineNum < boardSize = diagonalLineNum + 1
  | otherwise = boardSize - (diagonalLineNum - boardSize + 1)
    where
      diagonalLineNum = (lineNum - (boardSize * 2)) `mod` (boardSize * 2 - 1)

class CBitBoard a where
  horizontal :: a -> U.Vector Word64 -- all lines horizontaly
  vertical :: a -> U.Vector Word64   -- all lines verticaly
  diagonalL :: a -> U.Vector Word64  -- all diagonals starting from left upper field
  diagonalR :: a -> U.Vector Word64  -- all diagonals starting from right upper field
  getField :: a -> (Int, Int) -> Field
