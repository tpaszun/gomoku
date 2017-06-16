module Gomoku.BitBoard (
    BitBoard(..),
    createBitBoard,
    getLineLength,
    CBitBoard,
    horizontal,
    vertical,
    diagonalL,
    diagonalR,
    getField,
    getFieldHorizontal,
    getFieldVertical,
    getFieldDiagonalL,
    getFieldDiagonalR
) where

import Gomoku.Abstractions

import qualified Data.Vector.Unboxed as U
import Data.Word

data BitBoard = BitBoard {
  internalRep :: U.Vector Word64,
  boardLength :: Int
}

createBitBoard :: Int -> BitBoard
createBitBoard len = BitBoard {
  internalRep = U.replicate totalLength 0,
  boardLength = len
}
  where
    -- diagonalLength = len * 2 - 1
    -- horizontal + vertical + diagonalL + diagonalR
    -- totalLength = len + len + (len * 2) - 1 + (len * 2) - 1
    totalLength = len * 6 - 2

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
  horizontal :: a -> U.Vector Word64
  vertical :: a -> U.Vector Word64
  diagonalL :: a -> U.Vector Word64
  diagonalR :: a -> U.Vector Word64

  getField :: a -> (Int, Int) -> Field

  getFieldHorizontal :: a -> (Int, Int) -> Field
  getFieldVertical :: a -> (Int, Int) -> Field
  getFieldDiagonalL :: a -> (Int, Int) -> Field
  getFieldDiagonalR :: a -> (Int, Int) -> Field
