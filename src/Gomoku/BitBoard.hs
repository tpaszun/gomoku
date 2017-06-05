module Gomoku.BitBoard (
    BitBoard(..),
    createBitBoard,
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
  internalRep :: U.Vector Word32,
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


getLineLength :: BitBoard -> Int -> Int
getLineLength (BitBoard _ boardSize) lineNum
  | lineNum < boardSize * 2 = boardSize
  | diagonalLineNum < boardSize = diagonalLineNum + 1
  | otherwise = boardSize - (diagonalLineNum - boardSize + 1)
    where
      diagonalLineNum = (lineNum - (boardSize * 2)) `mod` (boardSize * 2 - 1)

class CBitBoard a where
  horizontal :: a -> U.Vector Word32
  vertical :: a -> U.Vector Word32
  diagonalL :: a -> U.Vector Word32
  diagonalR :: a -> U.Vector Word32

  getField :: a -> (Int, Int) -> Field