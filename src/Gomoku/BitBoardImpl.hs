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
  Store Board as a U.Vector Word64, where
    each line is Word64
    each field is 2 bits of Word64, where first field in list is least significant 2 bits
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
    Word64 - 64bit => max 32 fields


    Board stores:
      - horizontal lines
      - vertical lines
      - left diagonals
      - right diagonals
-}


instance Board BitBoard where

  blankBoard len = BitBoard {
    internalRep = U.replicate totalLength 0,
    boardLength = len
  }
    where
    -- diagonalLength = len * 2 - 1
    -- horizontal + vertical + diagonalL + diagonalR
    -- totalLength = len + len + (len * 2) - 1 + (len * 2) - 1
    totalLength = len * 6 - 2

  updateBoard bitboard (Move x y player) =
    bitboard {
      internalRep = (internalRep bitboard) U.// [
        (y, updatedHorizontalLine),
        (boardSize + x, updatedVerticalLine),
        (boardSize * 2 + diagonalLIndex, updatedDiagonalLLine),
        (boardSize * 4 - 1 + diagonalRIndex, updatedDiagonalRLine)
        ]
    }
    where
      boardSize = boardLength bitboard
      updatedHorizontalLine = setLineElem horizontalLine x (Player player)
        where horizontalLine = (horizontal bitboard) U.! y
      updatedVerticalLine = setLineElem verticalLine y (Player player)
        where verticalLine = (vertical bitboard) U.! x
      diagonalLIndex = x + y
      updatedDiagonalLLine = setLineElem diagonalLLine diagonalLField (Player player)
        where
          diagonalLLine = (diagonalL bitboard) U.! (diagonalLIndex)
          diagonalLField | diagonalLIndex < boardSize = y
                         | otherwise = y - (diagonalLIndex - boardSize + 1)
      diagonalRIndex = boardSize - 1 - (x - y)
      updatedDiagonalRLine = setLineElem diagonalRLine diagonalRField (Player player)
        where
          diagonalRLine = (diagonalR bitboard) U.! (diagonalRIndex)
          diagonalRField | diagonalRIndex < boardSize = y
                         | otherwise = x
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

intersection :: BitBoard -> (Int,Int) -> U.Vector (Int, Word64)
intersection board (x,y) =
    U.fromList [
      (boardSize, horiz),
      (boardSize, vert),
      (getLineLength board diagLLineNum, diagL),
      (getLineLength board diagRLineNum, diagR)]
  where
    boardSize = boardLength board
    horiz = (horizontal board) U.! y
    vert = (vertical board) U.! x
    diagLLineNum = boardSize * 2 + x + y
    diagL = (diagonalL board) U.! (x+y)
    diagRLineNum = boardSize * 5 - 2 - (x - y)
    diagR = (diagonalR board) U.! (boardSize - 1 - (x - y))

intersectionForMove :: BitBoard -> Move -> U.Vector (Int, Word64)
intersectionForMove board (Move x y player) =
    U.fromList [
      (boardSize, updatedHorizontalLine),
      (boardSize, updatedVerticalLine),
      (getLineLength board diagLLineNum, updatedDiagL),
      (getLineLength board diagRLineNum, updatedDiagR)]
  where
    boardSize = boardLength board
    horizontalLine = (horizontal board) U.! y
    updatedHorizontalLine = setLineElem horizontalLine x (Player player)
    verticalLine = (vertical board) U.! x
    updatedVerticalLine = setLineElem verticalLine y (Player player)
    diagLLineNum = boardSize * 2 + x + y
    diagL = (diagonalL board) U.! (x+y)
    updatedDiagL = setLineElem diagL diagonalLField (Player player)
      where
        diagonalLIndex = x + y
        diagonalLField | diagonalLIndex < boardSize = y
                       | otherwise = y - (diagonalLIndex - boardSize + 1)
    diagRLineNum = boardSize * 5 - 2 - (x - y)
    diagR = (diagonalR board) U.! (boardSize - 1 - (x - y))
    updatedDiagR = setLineElem diagR diagonalRField (Player player)
        where
          diagonalRIndex = boardSize - 1 - (x - y)
          diagonalRField | diagonalRIndex < boardSize = y
                         | otherwise = x

evaluateIntersection :: BitBoard -> (Int,Int) -> BoardEvaluation
evaluateIntersection board pos =
  BoardEvaluation {
    black = evaluatePlayer Black,
    white = evaluatePlayer White
  }
  where
    inters = intersection board pos
    evaluatePlayer player = PlayerEvaluation {
        fives = xss 5,
        fours = xss 4,
        threes = xss 3,
        doubles = xss 2
    }
      where
        xss n = openXsInIntersection inters player n

evaluateInters :: U.Vector (Int, Word64) -> BoardEvaluation
evaluateInters inters =
  BoardEvaluation {
    black = evaluatePlayer Black,
    white = evaluatePlayer White
  }
  where
    evaluatePlayer player = PlayerEvaluation {
        fives = xss 5,
        fours = xss 4,
        threes = xss 3,
        doubles = xss 2
    }
      where
        xss n = openXsInIntersection inters player n

evaluateIntersectionForMove :: BitBoard -> Move -> BoardEvaluation
evaluateIntersectionForMove board move =
  evaluateInters inters
  where
    inters = intersectionForMove board move


instance CBitBoard BitBoard where
  horizontal (BitBoard internal boardLength) = U.slice 0 boardLength internal
  vertical (BitBoard internal boardLength) = U.slice boardLength boardLength internal
  diagonalL (BitBoard internal boardLength) = U.slice (boardLength*2) (boardLength*2 - 1) internal
  diagonalR (BitBoard internal boardLength) = U.slice (boardLength*4 - 1) (boardLength*2 - 1) internal
  getField (BitBoard board _) (x,y) = lineElem (board U.! y) x


instance Show BitBoard where
  show bitboard = L.concat drawBoard
    where
      drawField :: Field -> Char
      drawField field = case field of
                          Player Black -> 'X'
                          Player White -> 'O'
                          Blank -> ' '
      drawLine :: Word64 -> String
      drawLine line = L.intersperse '|' $ fmap (drawField) $ lineToFieldList (boardLength bitboard) line
      drawBoard = (L.intersperse "\n" $ top : L.zipWith (\num line -> num : ' ' : line ) nums (L.map (drawLine) $ U.toList board))
        where
          board = horizontal bitboard

      nums = ['\9352'..]
      top = "  " ++ L.intersperse ' ' (take (boardLength bitboard) nums)
