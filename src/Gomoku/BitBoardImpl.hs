module Gomoku.BitBoardImpl where

import Gomoku.Abstractions
import Gomoku.BitBoard.Wizardry
import Gomoku.BitBoard
import Gomoku.BitBoard.PatternMatching


import qualified Data.Vector.Unboxed as U
import qualified Data.List as L
import Data.Word
import Data.Bits


{-
  Store Board as a U.Vector Word32, where
    each line is Word32
    each field is 2 bits of Word32, where first field in list is least significant 2 bits
    and last field in list most significant 2 bits
    ie. X|O| | |X|O|O|X -> 01 10 10 01 00 00 10 01 -> 0110100100001001 -> 26889 (decimal) -> 0x6909
        |           | |    |  |                 |
        |           | `----'  |                 |
        |           `---------'                 |
        `---------------------------------------'

    X - 01
    O - 10
    _ - 00

    Word32 - 32bit => max 16 fields
    For boards 16x16 < x <= 32x32 (19x19 particularly) Word32 should be changed to Word64


    Board stores:
      - horizontal lines
      - vertical lines
      - left diagonals
      - right diagonals

    "A left diagonal starts at the extreme left of the top row, while a right diagonal starts at the extreme right of the top row."
-}



instance Board BitBoard where
  blankBoard = createBitBoard
  updateBoard bitboard (Move x y player) =
    bitboard {
      internalRep = (internalRep bitboard) U.// [
        (y, updatedHorizontalLine),
        (boardLength bitboard + x, updatedVerticalLine)]
        -- TODO: update diagonals
    }
    where
      horizontalLine = (horizontal bitboard) U.! y
      updatedHorizontalLine = setLineElem horizontalLine x (Player player)
      verticalLine = (vertical bitboard) U.! x
      updatedVerticalLine = setLineElem verticalLine y (Player player)
  genMoves bitboard player =
    map (\(x,y) -> Move x y player) availableFields
      where
        len = boardLength bitboard
        availableFields = [(x, y)| x <- [0..(len - 1)],
                                   y <- [0..(len - 1)],
                                   getField bitboard (x, y) == Blank]
  genNeighboringMoves bitboard distance player =
    map (\(x,y) -> Move x y player) availableFields
    where
        len = boardLength bitboard
        availableFields = [(x, y)| x <- [0..(len - 1)],
                                   y <- [0..(len - 1)],
                                   getField bitboard (x, y) == Blank,
                                   let neighbours = [getField bitboard (neighbourX, neighbourY) | neighbourX <- [max (x-distance) 0..min (x+distance) (len-1)],
                                                                                                  neighbourY <- [max (y-distance) 0..min (y+distance) (len-1)]],
                                   any (/= Blank) neighbours]

  evaluatePlayer board player = PlayerEvaluation {
        fives = xss 5,
        fours = xss 4,
        threes = xss 3,
        doubles = xss 2
    }
      where
        xss n = openXsInBoard board player n

  evaluateBoard board = BoardEvaluation {
        black = evaluatePlayer board Black,
        white = evaluatePlayer board White
    }

-- evaluateMove :: BitBoard -> Move -> BoardEvaluation
-- evaluateMove board move =

--   where
--     evalPlayer p = layerEvaluation {
--         fives = xss 5,
--         fours = xss 4,
--         threes = xss 3,
--         doubles = xss 2
--     }
--       where
--         xss n = openXsInIntersection intersection size p x openXsInBoard board player n

--     intersection = U.fromList [
--       (size,                            (horizontal board) U.! x),
--       (size,                            (vertical board) U.! y),
--       (getLineLength board diagLindex,  diagL),
--       (getLineLength board diagRindex,  diagR) ]
--     diagLindex = 0
--     diagRindex = 0
--     (BitBoard internalRep size) = board
--     (Move x y player) = move
--     diagL = fromLine $
--             L.map (getField board) $
--             [(startX + d, startY + d) | let startX = if x < y then 0                 else x - y,
--                                         let startY = if x < y then y - x             else 0,
--                                         let len    = if x < y then size - startY - 1 else size - startX - 1,
--                                         d <- [0..len] ]

--     diagR = fromLine $
--             L.map (getField board) $
--             [(startX + d, startY - d) | let startX = if x + y < (size - 1) then 0          else x + y - size + 1,
--                                         let startY = if x + y < (size - 1) then x + y      else size - 1,
--                                         let len    = if x + y < (size - 1) then startY     else size - startX - 1,
--                                         d <- [0..len] ]


instance Show BitBoard where
  show bitboard = L.concat drawBoard
    where
      drawField :: Field -> Char
      drawField field = case field of
                          Player Black -> 'X'
                          Player White -> 'O'
                          Blank -> ' '
      drawLine :: Word32 -> String
      drawLine line = L.intersperse '|' $ fmap (drawField) $ lineToFieldList (boardLength bitboard) line
      drawBoard = L.intersperse "\n" $ L.map (drawLine) $ U.toList board
        where
          board = horizontal bitboard



instance CBitBoard BitBoard where
  horizontal (BitBoard internal boardLength) = U.slice 0 boardLength internal
  vertical (BitBoard internal boardLength) = U.slice boardLength boardLength internal
  diagonalL (BitBoard internal boardLength) = U.slice (boardLength*2) (boardLength*2 - 1) internal
  diagonalR (BitBoard internal boardLength) = U.slice (boardLength*4 - 1) (boardLength*2 - 1) internal

  getField (BitBoard board _) (x,y) = lineElem (board U.! y) x


